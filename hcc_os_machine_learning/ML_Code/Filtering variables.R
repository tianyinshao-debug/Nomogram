# 加载所需包
library(readr)
library(dplyr)
library(caret)
library(survival)
library(glmnet)

# ------------------------------
# 0. 定义路径和变量列表（根据用户最新输入）
# ------------------------------
data_dir <- "C:/data/rdata/mlm/"
train_path <- file.path(data_dir, "train_os_cart_imputed.csv")
test_path  <- file.path(data_dir, "test_os_cart_imputed.csv")

# 连续变量（修正了原列表中的多余逗号）
continuous_vars <- c("age", "bmi", "tumor_num", "tumor_dia", "portal_dia",
                     "splenn_dia", "hb", "wbc", "plt", "neu", "lym", "nlr", "pt", 
                     "alt", "ast", "tb", "tp", "tc", "alb", "glb",
                     "cr", "bun", "alp", "copy", "opt", "bleed", "zd_time")

# 二分类变量（包含 sec_inj, post_bld2, ..., post_trans）
binary_vars <- c("sex", "sgm", "dbt", "hpv", "hr_ev", "pvb", 
                 "meld10", "child_cl", "pvt", "hepatitis", 
                 "anti_vir", "ag1.5", "afp400", "hpm", "mhp", "opt_trans", "rm", 
                 "mvi", "satellite", "cpsule", "S4",  "sec_inj", "post_bld2", 
                 "post_bld3", "bil_fis3", "infection", "abd_eff2", "abd_eff3", 
                 "ple_eff2", "ple_eff3", "LF50", "yxss3", "ren_ins", "post_xhdcx", 
                 "opsi", "post_trans")

# 有序多分类变量
ordinal_vars <- c("asa", "es", "bclc")

# 所有预测变量
all_predictors <- c(continuous_vars, binary_vars, ordinal_vars)

cat("参与分析的预测变量共", length(all_predictors), "个：\n")
print(all_predictors)

# ------------------------------
# 1. 读取数据
# ------------------------------
train_full <- read_csv(train_path, show_col_types = FALSE)
test_external <- read_csv(test_path, show_col_types = FALSE)

# 检查数据中缺失的变量
missing_vars <- setdiff(all_predictors, names(train_full))
if(length(missing_vars) > 0) {
  warning("以下变量在数据中不存在：", paste(missing_vars, collapse = ", "))
  all_predictors <- intersect(all_predictors, names(train_full))
  cat("实际可用于建模的变量：", length(all_predictors), "个\n")
}

cat("训练集原始样本量：", nrow(train_full), "\n")

# ------------------------------
# 2. 拆分训练组(80%)和内部验证组(20%)，保证 suv 分层
# ------------------------------
set.seed(123)
split_index <- createDataPartition(train_full$suv, p = 0.8, list = FALSE)
train_set <- train_full[split_index, ]
valid_set <- train_full[-split_index, ]

cat("训练组样本量：", nrow(train_set), "\n")
cat("内部验证组样本量：", nrow(valid_set), "\n")

# ------------------------------
# 3. 数据预处理函数（支持训练集拟合参数并应用于其他数据集）
# ------------------------------
preprocess_data <- function(df, 
                            continuous_vars, 
                            binary_vars, 
                            ordinal_vars,
                            fit_params = NULL) {
  res <- df
  params <- list()
  
  # 二分类变量 -> 0/1
  for (var in binary_vars) {
    if (var %in% names(res)) {
      x <- res[[var]]
      if (is.factor(x) || is.character(x)) {
        lev <- unique(x)
        if (length(lev) == 2) {
          res[[var]] <- as.numeric(x == lev[2])
        } else {
          warning(paste(var, "不是二分类变量，将按因子水平编码（最小值为0）。"))
          res[[var]] <- as.numeric(as.factor(x)) - 1
        }
      } else if (is.numeric(x)) {
        uv <- unique(x)
        if (length(uv) == 2) {
          min_val <- min(uv)
          res[[var]] <- ifelse(x == min_val, 0, 1)
        } else {
          warning(paste(var, "数值型但值不唯一两个，请检查。"))
        }
      }
    }
  }
  
  # 有序多分类 -> 数值等级（1,2,3,...）
  for (var in ordinal_vars) {
    if (var %in% names(res)) {
      x <- res[[var]]
      if (is.ordered(x)) {
        res[[var]] <- as.numeric(x)
      } else if (is.factor(x) || is.character(x)) {
        # 保持原始水平顺序赋值为1:n
        x_fac <- factor(x, levels = unique(x))
        res[[var]] <- as.numeric(x_fac)
      } else if (is.numeric(x)) {
        res[[var]] <- x
      }
    }
  }
  
  # 连续变量：Z-score 归一化（若 fit_params 为 NULL 则计算均值和 sd）
  for (var in continuous_vars) {
    if (var %in% names(res)) {
      x <- res[[var]]
      if (is.null(fit_params)) {
        mn <- mean(x, na.rm = TRUE)
        sd <- sd(x, na.rm = TRUE)
        if (sd == 0) sd <- 1
        params[[var]] <- list(mean = mn, sd = sd)
        res[[var]] <- (x - mn) / sd
      } else {
        mn <- fit_params[[var]]$mean
        sd <- fit_params[[var]]$sd
        res[[var]] <- (x - mn) / sd
      }
    }
  }
  
  if (is.null(fit_params)) {
    return(list(data = res, params = params))
  } else {
    return(res)
  }
}

# 对训练组进行预处理（拟合）
preproc_train <- preprocess_data(train_set, continuous_vars, binary_vars, ordinal_vars)
train_processed <- preproc_train$data
norm_params <- preproc_train$params

# 对内部验证集和外部验证集应用相同变换
valid_processed <- preprocess_data(valid_set, continuous_vars, binary_vars, ordinal_vars, fit_params = norm_params)
test_processed <- preprocess_data(test_external, continuous_vars, binary_vars, ordinal_vars, fit_params = norm_params)

# ------------------------------
# 4. 提取训练组的预测变量矩阵和生存对象
# ------------------------------
X_train <- train_processed %>% select(all_of(all_predictors)) %>% as.matrix()
y_train <- Surv(train_processed$os, train_processed$suv)

# ------------------------------
# 5. LASSO-Cox 回归（5折交叉验证）
# ------------------------------
set.seed(456)
cv_fit <- cv.glmnet(x = X_train, 
                    y = y_train, 
                    family = "cox",
                    nfolds = 5,
                    alpha = 1,
                    type.measure = "deviance")

lambda_min <- cv_fit$lambda.min
lambda_1se <- cv_fit$lambda.1se
cat("\n最优 lambda.min =", lambda_min, "\n")
cat("最优 lambda.1se =", lambda_1se, "\n")

# 提取非零系数变量
coef_min <- coef(cv_fit, s = "lambda.min")
coef_min_nonzero <- as.matrix(coef_min)[which(coef_min != 0), , drop = FALSE]

coef_1se <- coef(cv_fit, s = "lambda.1se")
coef_1se_nonzero <- as.matrix(coef_1se)[which(coef_1se != 0), , drop = FALSE]

# ------------------------------
# 6. 保存结果到原路径
# ------------------------------
output_dir <- data_dir

if (nrow(coef_min_nonzero) > 0) {
  df_min <- data.frame(variable = rownames(coef_min_nonzero), 
                       coefficient = as.vector(coef_min_nonzero))
  write.csv(df_min, file.path(output_dir, "lasso_selected_lambda.min.csv"), row.names = FALSE)
  cat("\n✓ 已保存 lambda.min 筛选结果：", file.path(output_dir, "lasso_selected_lambda.min.csv"), "\n")
  print(df_min)
} else {
  cat("\n⚠ lambda.min 没有筛选出任何变量。\n")
}

if (nrow(coef_1se_nonzero) > 0) {
  df_1se <- data.frame(variable = rownames(coef_1se_nonzero), 
                       coefficient = as.vector(coef_1se_nonzero))
  write.csv(df_1se, file.path(output_dir, "lasso_selected_lambda.1se.csv"), row.names = FALSE)
  cat("✓ 已保存 lambda.1se 筛选结果：", file.path(output_dir, "lasso_selected_lambda.1se.csv"), "\n")
  print(df_1se)
} else {
  cat("⚠ lambda.1se 没有筛选出任何变量。\n")
}

# 保存交叉验证曲线图
png(file.path(output_dir, "lasso_cv_plot.png"), width = 800, height = 600)
plot(cv_fit)
dev.off()
cat("✓ 交叉验证曲线图已保存：", file.path(output_dir, "lasso_cv_plot.png"), "\n")

# 保存最优 lambda 值
writeLines(c(paste("lambda.min =", lambda_min), paste("lambda.1se =", lambda_1se)),
           con = file.path(output_dir, "optimal_lambda.txt"))
cat("✓ 最优 lambda 值已保存：", file.path(output_dir, "optimal_lambda.txt"), "\n")

# 可选：保存预处理后的数据集（便于后续分析）
write.csv(train_processed, file.path(output_dir, "train_processed.csv"), row.names = FALSE)
write.csv(valid_processed, file.path(output_dir, "internal_valid_processed.csv"), row.names = FALSE)
write.csv(test_processed, file.path(output_dir, "external_test_processed.csv"), row.names = FALSE)
cat("✓ 预处理后的数据已保存至相同目录。\n")