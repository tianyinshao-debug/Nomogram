# ============================================================
# 新病人预测：加载模型、标准化参数、基线生存函数
# 输出：风险分数、1/3/5年生存概率、中位生存时间（月）
# ============================================================

library(gbm)
library(survival)

# ------------------------------
# 1. 加载模型及相关文件
# ------------------------------
model_path <- "C:/data/rdata/mlm/gbm_model.rds"
scaling_path <- "C:/data/rdata/mlm/continuous_scaling_params.rds"
baseline_path <- "C:/data/rdata/mlm/baseline_surv.rds"

gbm_model <- readRDS(model_path)
scaling_params <- readRDS(scaling_path)   # 连续变量的均值和标准差
baseline <- readRDS(baseline_path)        # 数据框，列：time (月), survival

# ------------------------------
# 2. 定义新病人数据（示例）
#    请替换为实际值。注意列名与训练时完全一致。
# ------------------------------
new_patient <- data.frame(
  # 连续变量（原始值）
  tumor_num = 1,
  tumor_dia = 10,
  nlr = 2.8,
  glb = 32.5,
  bun = 6.1,
  copy = 200,
  # 二分类变量（已编码为0/1，如1=男性/阳性/存在等）
  sex = 1,
  sgm = 1,
  afp400 = 1,
  hpm = 0,
  rm = 0,
  mvi = 0,
  satellite = 0,
  cpsule = 0,
  LF50 = 0,
  # 有序变量（等级1~4）
  es = 4,
  stringsAsFactors = FALSE
)

# ------------------------------
# 3. 预处理：只标准化连续变量
# ------------------------------
preprocess_new_patient <- function(df, scaling_params) {
  res <- df
  for (var in names(scaling_params)) {
    if (var %in% names(res)) {
      mean_val <- scaling_params[[var]]$mean
      sd_val <- scaling_params[[var]]$sd
      res[[var]] <- (res[[var]] - mean_val) / sd_val
    } else {
      warning(paste("变量", var, "不存在于新病人数据中"))
    }
  }
  return(res)
}

new_patient_scaled <- preprocess_new_patient(new_patient, scaling_params)

# ------------------------------
# 4. 预测风险分数
# ------------------------------
risk_score <- predict(gbm_model, newdata = new_patient_scaled, 
                      n.trees = gbm_model$n.trees, type = "link")

# ------------------------------
# 5. 计算1/3/5年生存概率（单位：月）
# ------------------------------
times <- c(12, 36, 60)   # 12个月=1年，36个月=3年，60个月=5年
surv0_at_times <- approx(baseline$time, baseline$survival, xout = times, rule = 2)$y
surv_probs <- surv0_at_times ^ exp(risk_score)

# ------------------------------
# 6. 计算中位生存时间（月）
# ------------------------------
surv_curve <- baseline$survival ^ exp(risk_score)
idx <- which(surv_curve <= 0.5)[1]
if (!is.na(idx)) {
  median_surv <- baseline$time[idx]
} else {
  median_surv <- NA
  warning("中位生存时间超出基线时间范围")
}

# ------------------------------
# 7. 输出结果
# ------------------------------
cat("\n========== 预测结果 ==========\n")
cat(sprintf("相对风险分数 (Risk Score): %.4f\n", risk_score))
cat(sprintf("1年生存概率 (12个月): %.4f (%.1f%%)\n", surv_probs[1], surv_probs[1] * 100))
cat(sprintf("3年生存概率 (36个月): %.4f (%.1f%%)\n", surv_probs[2], surv_probs[2] * 100))
cat(sprintf("5年生存概率 (60个月): %.4f (%.1f%%)\n", surv_probs[3], surv_probs[3] * 100))
if (!is.na(median_surv)) {
  cat(sprintf("中位生存时间: %.1f 个月 (约 %.1f 年)\n", median_surv, median_surv / 12))
} else {
  cat("中位生存时间: 未达到（超出观察期）\n")
}