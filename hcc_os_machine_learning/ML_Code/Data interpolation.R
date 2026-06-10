# ============================================
# MICE-CART 缺失值插补完整代码（修复版）
# 处理：变量不存在、类型不一致、验证集Transform
# ============================================

# 1. 加载必要的包
library(mice)        
library(readxl)      
library(rpart)       
library(dplyr)       

# 2. 设置路径
train_path <- "C:/data/rdata/mlm/train_os.xls"
test_path  <- "C:/data/rdata/mlm/test_os.xls"

# 3. 定义变量分类（基于诊断报告）
continuous_vars <- c("age", "bmi", "tumor_num", "tumor_dia", "portal_dia",
                     "splenn_dia", "hb", "wbc", "plt", "neu", "lym", "pt", 
                     "inr", "alt", "ast", "tb", "tp", "tc", "alb", "glb", 
                     "cr", "bun", "alp", "copy", "opt", "bleed", "zd_time")

binary_vars <- c("sex", "sgm", "dbt", "hpv", "red_sign", "hr_ev", "pvb", 
                 "meld10", "mat16.3", "cupi_cl", "child_cl", "pvt", "hepatitis", 
                 "anti_vir", "ag1.5", "afp400", "hpm", "mhp", "opt_trans", "rm", 
                 "mvi", "satellite", "cpsule", "S4", "sec_inj", "post_bld2", 
                 "post_bld3", "bil_fis3", "infection", "abd_eff2", "abd_eff3", 
                 "ple_eff2", "ple_eff3", "LF50", "yxss3", "ren_ins", "post_xhdcx", 
                 "opsi", "post_trans", "post_pvt", "pmd")

multiclass_vars <- c("asa", "egv", "es", "vp", "bclc")

exclude_vars <- c("nub", "suv", "os")

# 原始需要插补的目标变量（后续会根据实际数据筛选）
impute_vars_original <- c("bun", "bmi", "splenn_dia", "portal_dia", "neu", "lym", 
                          "tc", "zd_time", "copy", "mvi", "cpsule", "satellite", 
                          "rm", "red_sign", "es", "egv")

# 4. 读取数据
train_raw <- read_excel(train_path, na = "NA")
test_raw  <- read_excel(test_path, na = "NA")

cat("========================================\n")
cat("数据读取完成\n")
cat("训练集维度:", dim(train_raw), "| 验证集维度:", dim(test_raw), "\n")
cat("训练集变量:", paste(names(train_raw), collapse = ", "), "\n")
cat("验证集变量:", paste(names(test_raw), collapse = ", "), "\n")
cat("========================================\n\n")

# 5. 数据预处理与类型转换（增强版）
# ============================================

preprocess_data <- function(df, dataset_name) {
  
  cat("【", dataset_name, "】数据预处理...\n", sep = "")
  
  df_new <- df
  
  # 处理character类型变量 → 强制转为numeric（NA表示转换失败）
  char_vars <- names(df_new)[sapply(df_new, is.character)]
  if(length(char_vars) > 0) {
    cat("  检测到字符型变量:", paste(char_vars, collapse = ", "), "→ 转换为numeric\n")
    for(var in char_vars) {
      df_new[[var]] <- suppressWarnings(as.numeric(as.character(df_new[[var]])))
    }
  }
  
  # 连续变量 → numeric
  for(var in continuous_vars) {
    if(var %in% names(df_new)) {
      df_new[[var]] <- as.numeric(as.character(df_new[[var]]))
    }
  }
  
  # 二分类变量 → factor
  for(var in binary_vars) {
    if(var %in% names(df_new)) {
      unique_vals <- sort(unique(na.omit(as.numeric(as.character(df_new[[var]])))))
      df_new[[var]] <- factor(as.character(df_new[[var]]), levels = as.character(unique_vals))
    }
  }
  
  # 多分类变量 → factor
  for(var in multiclass_vars) {
    if(var %in% names(df_new)) {
      unique_vals <- sort(unique(na.omit(as.numeric(as.character(df_new[[var]])))))
      df_new[[var]] <- factor(as.character(df_new[[var]]), levels = as.character(unique_vals))
    }
  }
  
  cat("  类型转换完成。变量数:", ncol(df_new), "\n\n")
  return(df_new)
}

train_data <- preprocess_data(train_raw, "训练集")
test_data  <- preprocess_data(test_raw, "验证集")

# 6. 动态确定实际存在的插补变量
# ============================================
# 关键修复：只保留数据集中实际存在的变量

impute_vars <- intersect(impute_vars_original, names(train_data))
cat("实际存在的需要插补的变量 (", length(impute_vars), "个):\n", sep = "")
cat(paste(impute_vars, collapse = ", "), "\n")

missing_in_train <- setdiff(impute_vars_original, names(train_data))
missing_in_test  <- setdiff(impute_vars_original, names(test_data))

if(length(missing_in_train) > 0) {
  cat("\n⚠ 训练集中不存在的变量:", paste(missing_in_train, collapse = ", "), "\n")
}
if(length(missing_in_test) > 0) {
  cat("⚠ 验证集中不存在的变量:", paste(missing_in_test, collapse = ", "), "\n")
}
cat("\n")

# 7. 输出缺失值项目与数值类型报告
# ============================================

cat("╔══════════════════════════════════════════════════════════════════════╗\n")
cat("║              缺失值项目与数值类型报告（预处理后的R类型）                ║\n")
cat("╚══════════════════════════════════════════════════════════════════════╝\n\n")

generate_cart_report <- function(df, dataset_name) {
  
  cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
  cat("【", dataset_name, "】缺失值与CART插补适用类型\n", sep = "")
  cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n")
  
  report <- data.frame(
    Variable = names(df),
    R_Class = sapply(df, function(x) paste(class(x), collapse = "/")),
    N_Total = nrow(df),
    N_Missing = sapply(df, function(x) sum(is.na(x))),
    Missing_Rate_Pct = round(sapply(df, function(x) sum(is.na(x))) / nrow(df) * 100, 2),
    CART_Type = sapply(df, function(x) {
      if(is.factor(x)) "Classification Tree" else "Regression Tree"
    }),
    Need_Impute = sapply(names(df), function(v) {
      if(v %in% impute_vars) "★ YES" else if(v %in% exclude_vars) "- Excluded" else "No"
    }),
    stringsAsFactors = FALSE
  ) %>%
    arrange(desc(Missing_Rate_Pct))
  
  # 只显示有缺失或需要插补的变量
  display_report <- report %>% 
    filter(N_Missing > 0 | Need_Impute == "★ YES") %>%
    select(Variable, R_Class, N_Missing, Missing_Rate_Pct, CART_Type, Need_Impute)
  
  cat("一、存在缺失值或需要插补的变量:\n")
  cat("────────────────────────────────────────────────────────────────────\n")
  print(display_report, row.names = FALSE)
  cat("\n")
  
  # 按CART类型分组
  cat("二、按CART树类型分组的缺失统计:\n")
  cat("────────────────────────────────────────────────────────────────────\n")
  cart_summary <- report %>%
    filter(N_Missing > 0) %>%
    group_by(CART_Type) %>%
    summarise(
      Var_Count = n(),
      Total_Missing = sum(N_Missing),
      Max_Rate = max(Missing_Rate_Pct),
      .groups = 'drop'
    )
  print(cart_summary, row.names = FALSE)
  cat("\n")
  
  # 详细类型输出
  cat("三、需要插补变量的CART处理详情:\n")
  cat("────────────────────────────────────────────────────────────────────\n")
  
  imp_vars <- intersect(impute_vars, names(df))
  for(var in imp_vars) {
    x <- df[[var]]
    r_class <- paste(class(x), collapse = "/")
    n_miss <- sum(is.na(x))
    miss_pct <- round(n_miss / length(x) * 100, 2)
    cart_type <- if(is.factor(x)) "分类树 (Classification)" else "回归树 (Regression)"
    
    if(is.factor(x)) {
      levels_info <- paste(levels(x), collapse = ", ")
      cat(sprintf("  %-12s | %-10s | 缺失: %3d (%5.2f%%) | %s | 水平: %s\n", 
                  var, r_class, n_miss, miss_pct, cart_type, levels_info))
    } else {
      range_info <- if(n_miss < length(x)) {
        sprintf("[%.2f, %.2f]", min(x, na.rm = TRUE), max(x, na.rm = TRUE))
      } else "All NA"
      cat(sprintf("  %-12s | %-10s | 缺失: %3d (%5.2f%%) | %s | 范围: %s\n", 
                  var, r_class, n_miss, miss_pct, cart_type, range_info))
    }
  }
  cat("\n")
  
  return(report)
}

train_report <- generate_cart_report(train_data, "训练集")
test_report  <- generate_cart_report(test_data, "验证集")

# 保存报告
write.csv(train_report, "C:/data/rdata/mlm/train_cart_report.csv", row.names = FALSE)
write.csv(test_report, "C:/data/rdata/mlm/test_cart_report.csv", row.names = FALSE)

# 8. 构建MICE-CART参数
# ============================================

cat("╔══════════════════════════════════════════════════════════════════════╗\n")
cat("║                    MICE-CART 插补参数配置                             ║\n")
cat("╚══════════════════════════════════════════════════════════════════════╝\n\n")

# 构建方法向量：只针对实际存在的、有缺失的变量
method_vec <- make.method(train_data)
method_vec[] <- ""

for(var in impute_vars) {
  if(var %in% names(train_data)) {
    # 检查训练集或验证集是否有缺失
    train_has_na <- sum(is.na(train_data[[var]])) > 0
    test_has_na  <- if(var %in% names(test_data)) sum(is.na(test_data[[var]])) > 0 else FALSE
    
    if(train_has_na || test_has_na) {
      method_vec[var] <- "cart"
    }
  }
}

cat("插补方法设置 (CART):\n")
cart_methods <- data.frame(
  Variable = names(method_vec[method_vec != ""]),
  Method = method_vec[method_vec != ""],
  Type = sapply(names(method_vec[method_vec != ""]), function(v) {
    if(is.factor(train_data[[v]])) "Classification Tree" else "Regression Tree"
  }),
  stringsAsFactors = FALSE
)
print(cart_methods, row.names = FALSE)
cat("\n")

# 构建预测矩阵
pred_mat <- make.predictorMatrix(train_data)

# 排除变量不作为预测因子
for(ex_var in exclude_vars) {
  if(ex_var %in% names(train_data)) {
    pred_mat[, ex_var] <- 0
  }
}

# 不需要插补的变量也不被插补
for(var in names(train_data)) {
  if(!var %in% impute_vars) {
    pred_mat[var, ] <- 0
  }
}

cat("预测矩阵: ", nrow(pred_mat), " x ", ncol(pred_mat), "\n")
cat("参与插补的变量数: ", sum(rowSums(pred_mat) > 0), "\n\n")

# 9. 训练集 MICE-CART 插补
# ============================================

cat("╔══════════════════════════════════════════════════════════════════════╗\n")
cat("║              训练集 MICE-CART 插补 (m=1, maxit=10)                    ║\n")
cat("╚══════════════════════════════════════════════════════════════════════╝\n\n")

set.seed(12345)

train_mice <- mice(
  data = train_data,
  method = method_vec,
  predictorMatrix = pred_mat,
  m = 1,
  maxit = 10,
  seed = 12345,
  printFlag = TRUE,
  visitSequence = "roman"
)

cat("\n训练集插补完成!\n")

# 获取插补后的完整训练集
train_completed <- complete(train_mice, action = 1)

# 验证训练集插补效果
cat("\n训练集插补验证:\n")
cat("────────────────────────────────────────────────────────────────────\n")
for(var in impute_vars) {
  if(var %in% names(train_data)) {
    before <- sum(is.na(train_data[[var]]))
    after <- sum(is.na(train_completed[[var]]))
    cat(sprintf("  %-12s: 插补前 %3d → 插补后 %3d [%s]\n", 
                var, before, after, ifelse(after == 0, "✓", "✗")))
  }
}

# 10. 验证集 Transform（修复版）
# ============================================

cat("\n╔══════════════════════════════════════════════════════════════════════╗\n")
cat("║              验证集插补：使用训练集CART模型Transform                    ║\n")
cat("╚══════════════════════════════════════════════════════════════════════╝\n\n")

# 步骤1：对齐验证集变量（只保留训练集存在的变量）
common_vars <- intersect(names(train_data), names(test_data))
test_aligned <- test_data[, common_vars, drop = FALSE]

# 补充训练集有但验证集缺失的变量（设为NA）
for(var in names(train_data)) {
  if(!var %in% names(test_aligned)) {
    test_aligned[[var]] <- NA
  }
}

# 按训练集变量顺序排列，并确保类型一致
test_aligned <- test_aligned[, names(train_data), drop = FALSE]

# 关键修复：确保验证集变量类型与训练集一致
cat("验证集类型对齐检查:\n")
for(var in names(train_data)) {
  if(var %in% names(test_aligned)) {
    train_class <- class(train_data[[var]])
    test_class <- class(test_aligned[[var]])
    
    if(!identical(train_class, test_class)) {
      cat(sprintf("  %-12s: 训练集 %s → 验证集 %s (正在对齐...)\n", 
                  var, paste(train_class, collapse = "/"), paste(test_class, collapse = "/")))
      
      if(is.factor(train_data[[var]])) {
        # 训练集是factor，验证集也转为factor（使用训练集的水平）
        test_aligned[[var]] <- factor(as.character(test_aligned[[var]]), 
                                      levels = levels(train_data[[var]]))
      } else {
        # 训练集是numeric，验证集也转为numeric
        test_aligned[[var]] <- as.numeric(as.character(test_aligned[[var]]))
      }
    }
  }
}
cat("类型对齐完成。\n\n")

# 步骤2：合并训练集（已插补）+ 验证集（待插补）
combined_data <- rbind(train_completed, test_aligned)

# 步骤3：构建where矩阵 —— 只标记验证集的缺失值
combined_where <- is.na(combined_data)
combined_where[1:nrow(train_completed), ] <- FALSE

for(var in names(combined_data)) {
  if(!var %in% impute_vars) {
    combined_where[, var] <- FALSE
  }
}

cat("验证集待插补统计:\n")
cat("────────────────────────────────────────────────────────────────────\n")
for(var in impute_vars) {
  if(var %in% names(test_aligned)) {
    n_miss <- sum(is.na(test_aligned[[var]]))
    n_to_imp <- sum(combined_where[(nrow(train_completed)+1):nrow(combined_data), var])
    cat(sprintf("  %-12s: 原始缺失 %3d | 待插补 %3d\n", var, n_miss, n_to_imp))
  }
}

# 步骤4：运行MICE-CART，只更新验证集
cat("\n开始验证集CART transform插补...\n")

combined_mice <- mice(
  data = combined_data,
  method = method_vec,
  predictorMatrix = pred_mat,
  where = combined_where,
  visitSequence = impute_vars,
  m = 1,
  maxit = 1,
  seed = 12345,
  printFlag = TRUE
)

# 提取验证集结果
test_completed <- complete(combined_mice, action = 1)[(nrow(train_completed)+1):nrow(combined_data), ]

cat("\n验证集插补完成!\n")

# 11. 最终验证与保存
# ============================================

cat("\n╔══════════════════════════════════════════════════════════════════════╗\n")
cat("║                         最终验证与结果保存                             ║\n")
cat("╚══════════════════════════════════════════════════════════════════════╝\n\n")

# 训练集最终检查
cat("【训练集】插补结果:\n")
cat("────────────────────────────────────────────────────────────────────\n")
train_check <- data.frame(
  Variable = impute_vars,
  Before = sapply(train_data[impute_vars], function(x) sum(is.na(x))),
  After = sapply(train_completed[impute_vars], function(x) sum(is.na(x))),
  stringsAsFactors = FALSE
)
train_check$Status <- ifelse(train_check$After == 0, "✓ Complete", "✗ Failed")
print(train_check, row.names = FALSE)

# 验证集最终检查
cat("\n【验证集】插补结果:\n")
cat("────────────────────────────────────────────────────────────────────\n")
test_check <- data.frame(
  Variable = impute_vars,
  Before = sapply(test_data[impute_vars], function(x) sum(is.na(x))),
  After = sapply(test_completed[impute_vars], function(x) sum(is.na(x))),
  stringsAsFactors = FALSE
)
test_check$Status <- ifelse(test_check$After == 0, "✓ Complete", "✗ Failed")
print(test_check, row.names = FALSE)

# 验证集全量检查
cat("\n验证集全部变量缺失值检查:\n")
test_all_miss <- sapply(test_completed, function(x) sum(is.na(x)))
remaining_miss <- test_all_miss[test_all_miss > 0]
if(length(remaining_miss) == 0) {
  cat("  ✓ 验证集所有变量已无缺失值\n")
} else {
  cat("  ⚠ 以下变量仍有缺失:\n")
  print(remaining_miss)
}

# 保存结果
cat("\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
cat("保存插补结果\n")
cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")

write.csv(train_completed, "C:/data/rdata/mlm/train_os_cart_imputed.csv", row.names = FALSE)
write.csv(test_completed, "C:/data/rdata/mlm/test_os_cart_imputed.csv", row.names = FALSE)
saveRDS(train_mice, "C:/data/rdata/mlm/train_mice_cart_model.rds")

cat("\n输出文件:\n")
cat("  1. 训练集插补结果: C:/data/rdata/mlm/train_os_cart_imputed.csv\n")
cat("  2. 验证集插补结果: C:/data/rdata/mlm/test_os_cart_imputed.csv\n")
cat("  3. CART模型对象:    C:/data/rdata/mlm/train_mice_cart_model.rds\n")
cat("  4. 诊断报告:        C:/data/rdata/mlm/*_cart_report.csv\n")

cat("\n╔══════════════════════════════════════════════════════════════════════╗\n")
cat("║                    MICE-CART 插补流程全部完成                           ║\n")
cat("╚══════════════════════════════════════════════════════════════════════╝\n")