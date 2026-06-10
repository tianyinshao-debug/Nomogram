# =============================================================================
# DCA 对比：分别计算每个模型的决策曲线（外部验证集，连续风险评分）
# 时间点：12/36/60 个月
# 字体：Times New Roman (使用 serif 族)
# =============================================================================
library(rmda)
library(survival)
library(readr)

# ------------------------------ 0. 路径与已有对象 ------------------------------
if (!exists("gbm_fit")) {
  gbm_fit <- readRDS(file.path(out_dir, "gbm_model.rds"))
}

data_dir <- "C:/data/rdata/mlm/"
external_path <- file.path(data_dir, "external_test_processed.csv")
test_df <- read_csv(external_path, show_col_types = FALSE)

required_cols <- c("os", "suv", "non", "MAT", "cupi")
missing <- required_cols[!required_cols %in% names(test_df)]
if (length(missing) > 0) {
  stop("外部验证集缺少以下列：", paste(missing, collapse = ", "))
}
test_df$suv <- as.numeric(test_df$suv)

# 构建 X_test
lasso_path <- file.path(data_dir, "lasso_selected_lambda.min.csv")
lasso_vars <- read_csv(lasso_path, show_col_types = FALSE)
selected_vars <- lasso_vars$variable
X_test <- as.matrix(test_df[, selected_vars])

# ------------------------------ GBM 风险预测函数 ------------------------------
predict_gbm_risk <- function(model, X_data, time_point) {
  X_df <- as.data.frame(X_data)
  colnames(X_df) <- colnames(X_data)
  lp <- predict.gbm(model, newdata = X_df, n.trees = model$n.trees, type = "link")
  lp <- as.numeric(lp)
  risk_offset <- exp(lp - model$train_risk_mean)
  idx <- max(which(model$base_haz$time <= time_point))
  if (length(idx) == 0) cum_haz <- 0 else cum_haz <- model$base_haz$hazard[idx]
  surv_prob <- exp(-cum_haz * risk_offset)
  return(1 - surv_prob)
}

# ------------------------------ 绘制 DCA 函数（每个时间点）-----------------------------
time_points <- c(12, 36, 60)

for (tp in time_points) {
  cat("\n===== 绘制", tp, "个月 DCA（分别计算各模型）=====\n")
  
  # 计算 GBM 风险概率
  gbm_risk <- predict_gbm_risk(gbm_fit, X_test, tp)
  
  # 创建包含各模型风险概率的数据框
  plot_df <- data.frame(
    event = test_df$suv,
    GBM = gbm_risk,
    Non = test_df$non,
    MAT = test_df$MAT,
    cupi = test_df$cupi
  )
  plot_df <- na.omit(plot_df)
  
  # 分别构建每个模型的 decision_curve
  dca_GBM <- decision_curve(event ~ GBM, data = plot_df, 
                            thresholds = seq(0, 0.5, by = 0.01),
                            policy = "opt-in", bootstraps = 100)
  dca_Non <- decision_curve(event ~ Non, data = plot_df,
                            thresholds = seq(0, 0.5, by = 0.01),
                            policy = "opt-in", bootstraps = 100)
  dca_MAT <- decision_curve(event ~ MAT, data = plot_df,
                            thresholds = seq(0, 0.5, by = 0.01),
                            policy = "opt-in", bootstraps = 100)
  dca_cupi <- decision_curve(event ~ cupi, data = plot_df,
                             thresholds = seq(0, 0.5, by = 0.01),
                             policy = "opt-in", bootstraps = 100)
  
  # Treat All: 恒为1
  all_df <- plot_df
  all_df$all <- 1
  dca_all <- decision_curve(event ~ all, data = all_df,
                            thresholds = seq(0, 0.5, by = 0.01),
                            policy = "opt-in", bootstraps = 100)
  
  # Treat None: 恒为0
  none_df <- plot_df
  none_df$none <- 0
  dca_none <- decision_curve(event ~ none, data = none_df,
                             thresholds = seq(0, 0.5, by = 0.01),
                             policy = "opt-in", bootstraps = 100)
  
  # 整合所有曲线对象
  curve_list <- list(dca_GBM, dca_Non, dca_MAT, dca_cupi, dca_all, dca_none)
  curve_names <- c("GBM", "Nomogram", "MELD-AFP-TBS", "CUPI", "Treat All", "Treat None")
  curve_cols  <- c("#1F77B4", "#E41A1C", "#4DAF4A", "#984EA3", "black", "grey")
  curve_lty   <- c(1, 1, 1, 1, 2, 2)
  curve_lwd   <- c(2, 2, 2, 2, 1.5, 1.5)
  
  # 设置全局字体为 Times New Roman（serif）
  pdf_file <- file.path(out_dir, sprintf("DCA_GBM_Non_MAT_cupi_test_%dm.pdf", tp))
  png_file <- file.path(out_dir, sprintf("DCA_GBM_Non_MAT_cupi_test_%dm.png", tp))
  
  # PDF 设备
  pdf(pdf_file, width = 7, height = 6, family = "serif")  # 直接指定 family
  par(family = "serif")  # 确保所有文本元素使用 serif
  plot_decision_curve(curve_list, curve.names = curve_names,
                      col = curve_cols, lty = curve_lty, lwd = curve_lwd,
                      xlim = c(0, 0.5), ylim = c(-0.05, 0.4),
                      cost.benefit.axis = FALSE,
                      confidence.intervals = FALSE,
                      standardize = FALSE,
                      main = sprintf("Decision Curve Analysis - External Validation Set (%d months)", tp),
                      xlab = "Threshold Probability", ylab = "Net Benefit")
  legend("topright", legend = curve_names, col = curve_cols, lty = curve_lty, lwd = curve_lwd, bty = "n")
  dev.off()
  
  # PNG 设备（高分辨率）
  png(png_file, width = 7, height = 6, units = "in", res = 300, family = "serif")
  par(family = "serif")
  plot_decision_curve(curve_list, curve.names = curve_names,
                      col = curve_cols, lty = curve_lty, lwd = curve_lwd,
                      xlim = c(0, 0.5), ylim = c(-0.05, 0.4),
                      cost.benefit.axis = FALSE,
                      confidence.intervals = FALSE,
                      standardize = FALSE,
                      main = sprintf("Decision Curve Analysis - External Validation Set (%d months)", tp),
                      xlab = "Threshold Probability", ylab = "Net Benefit")
  legend("topright", legend = curve_names, col = curve_cols, lty = curve_lty, lwd = curve_lwd, bty = "n")
  dev.off()
  
  cat(sprintf("已保存 %d 个月 DCA：\n  %s\n  %s\n", tp, pdf_file, png_file))
}

cat("\n===== 所有 DCA 绘制完成！=====\n")