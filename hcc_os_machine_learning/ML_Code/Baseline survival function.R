# ============================================================
# 生成基线生存函数（手动定义特征列）
# 时间单位：月
# ============================================================
library(gbm)
library(survival)

# 路径
model_path <- "C:/data/rdata/mlm/gbm_model.rds"
train_path <- "C:/data/rdata/mlm/train_processed.csv"

# 读取模型和训练数据
gbm_model <- readRDS(model_path)
train_data <- read.csv(train_path, stringsAsFactors = FALSE)

# ------------------------------
# 手动定义特征列（与模型训练时使用的完全一致）
# ------------------------------
continuous_vars <- c("tumor_num", "tumor_dia", "nlr", "glb", "bun", "copy")
binary_vars <- c("sex", "sgm", "afp400", "hpm", "rm", "mvi", "satellite", "cpsule", "LF50")
ordinal_vars <- c("es")

# 合并所有特征列名
feature_names <- c(continuous_vars, binary_vars, ordinal_vars)

# 提取特征矩阵（确保所有特征都存在）
missing_vars <- setdiff(feature_names, names(train_data))
if (length(missing_vars) > 0) {
  stop("训练数据缺少以下特征列：", paste(missing_vars, collapse = ", "))
}
train_features <- train_data[, feature_names, drop = FALSE]

# ------------------------------
# 计算训练集风险分数
# ------------------------------
train_risk <- predict(gbm_model, newdata = train_features, 
                      n.trees = gbm_model$n.trees, type = "link")

# ------------------------------
# 拟合 Cox 模型（偏移量）
# ------------------------------
train_data$risk_score <- train_risk
cox_fit <- coxph(Surv(os, suv) ~ offset(risk_score), data = train_data)

# ------------------------------
# 提取基线累积风险并转换为生存概率
# ------------------------------
baseline_cumhaz <- basehaz(cox_fit, centered = FALSE)
baseline_surv <- data.frame(
  time = baseline_cumhaz$time,
  survival = exp(-baseline_cumhaz$hazard)
)

# ------------------------------
# 精细插值（步长 0.1 个月）
# ------------------------------
max_os <- max(train_data$os, na.rm = TRUE)
fine_grid <- seq(0, max_os, by = 0.1)
surv_interp <- approx(baseline_surv$time, baseline_surv$survival, 
                      xout = fine_grid, rule = 2)$y
baseline_fine <- data.frame(time = fine_grid, survival = surv_interp)

# ------------------------------
# 保存基线生存函数
# ------------------------------
saveRDS(baseline_fine, "C:/data/rdata/mlm/baseline_surv.rds")
cat("基线生存函数已保存（单位：月），时间范围 0 ～", max_os, "个月\n")