# =========================================================================
# GBM 模型外部验证集校准曲线（12、36、60 个月）
# 方法：rms::calibrate，Bootstrap = 500，分组数 = 6，KM 估计实际生存率
# 依赖：训练好并包含 base_haz 和 train_risk_mean 的 gbm_model.rds
# =========================================================================

# ------------------------------ 0. 加载包 --------------------------------
library(readr)
library(survival)
library(dplyr)
library(gbm)
library(rms)          # 用于 calibrate 函数
library(ggplot2)      # 可选，用于更美观的保存（本脚本使用基础绘图）

# ------------------------------ 1. 路径设置（请根据实际情况修改）----------
data_dir <- "C:/data/rdata/mlm/"
out_dir  <- file.path(data_dir, "ModelResults_FullIBS_3timepoints")

train_path      <- file.path(data_dir, "train_processed.csv")
external_path   <- file.path(data_dir, "external_test_processed.csv")
lasso_path      <- file.path(data_dir, "lasso_selected_lambda.min.csv")
gbm_model_path  <- file.path(out_dir, "gbm_model.rds")

# 输出图片路径（PDF 和 PNG）
pdf_dir <- out_dir
png_dir <- out_dir

# ------------------------------ 2. 读取数据 ------------------------------
# 外部验证集
test_df <- read_csv(external_path, show_col_types = FALSE)

# LASSO 筛选的变量（用于保证特征一致性）
lasso_vars <- read_csv(lasso_path, show_col_types = FALSE)
selected_vars <- lasso_vars$variable

# 检查变量是否存在
stopifnot(all(selected_vars %in% names(test_df)))
stopifnot(all(c("os", "suv") %in% names(test_df)))

# 构建外部验证集特征矩阵和生存对象
X_test <- as.matrix(test_df[, selected_vars])
y_test <- Surv(time = test_df$os, event = test_df$suv)

cat("外部验证集样本量：", nrow(test_df), "\n")
cat("事件数（死亡）：", sum(test_df$suv), "\n")
cat("最大随访时间（月）：", max(test_df$os), "\n\n")

# ------------------------------ 3. 加载 GBM 模型 -------------------------
gbm_fit <- readRDS(gbm_model_path)

# 检查模型必需组件
if (is.null(gbm_fit$base_haz) || is.null(gbm_fit$train_risk_mean)) {
  stop("GBM 模型缺少 base_haz 或 train_risk_mean，请重新训练并将这些属性保存到模型中。")
}
if (is.null(gbm_fit$n.trees)) {
  stop("GBM 模型缺少 n.trees，请检查模型文件。")
}
cat("GBM 模型加载成功，n.trees =", gbm_fit$n.trees, "\n\n")

# ------------------------------ 4. 计算 GBM 风险评分（线性预测值）--------
# 注意：gbm 的 type="link" 返回的是线性预测值（相当于 Cox 模型的 LP）
X_test_df <- as.data.frame(X_test)
colnames(X_test_df) <- colnames(X_test)

risk_gbm <- predict.gbm(gbm_fit,
                        newdata = X_test_df,
                        n.trees = gbm_fit$n.trees,
                        type = "link")
test_df$risk_gbm <- as.numeric(risk_gbm)
cat("GBM 风险评分计算完成，范围：", range(test_df$risk_gbm), "\n\n")

# ------------------------------ 5. 准备 rms 环境 -------------------------
dd <- datadist(test_df)
options(datadist = "dd")

# 拟合 Cox 模型（以风险评分为唯一协变量）
cph_model <- cph(Surv(os, suv) ~ risk_gbm, data = test_df,
                 x = TRUE, y = TRUE, surv = TRUE)
cat("Cox 模型拟合完成。\n\n")

# ------------------------------ 6. 校准曲线参数 -------------------------
n_groups <- 6          # 分位数分组数
n_bootstrap <- 500     # Bootstrap 重采样次数
time_points <- c(12, 36, 60)

# 检查每个时间点是否超过最大随访时间
max_fu <- max(test_df$os, na.rm = TRUE)
if (any(time_points > max_fu)) {
  warning("以下时间点超过最大随访时间 (", max_fu, " 个月)：",
          paste(time_points[time_points > max_fu], collapse = ", "),
          "，将跳过这些时间点。")
  time_points <- time_points[time_points <= max_fu]
}
if (length(time_points) == 0) stop("没有有效的时间点可绘图。")

# 计算分组大小（近似）
n_total <- nrow(test_df)
m_size <- round(n_total / n_groups)
cat("校准曲线参数：\n")
cat("  分组数：", n_groups, "（每组约", m_size, "例）\n")
cat("  Bootstrap 次数：", n_bootstrap, "\n")
cat("  时间点（月）：", paste(time_points, collapse = ", "), "\n\n")

# ------------------------------ 7. 分别计算校准曲线并绘图 ----------------
# 设置颜色（可选）
colors <- c("red", "blue", "darkgreen")
names(colors) <- time_points

for (tp in time_points) {
  cat("\n===== 正在计算", tp, "个月的校准曲线 (Bootstrap ", n_bootstrap, "次) =====\n")
  
  # 使用 calibrate 函数
  cal_obj <- tryCatch({
    calibrate(cph_model,
              u = tp,                # 时间点
              cmethod = "KM",        # 使用 Kaplan-Meier 估计实际生存率
              m = m_size,            # 每组样本量
              B = n_bootstrap)       # Bootstrap 次数
  }, error = function(e) {
    cat("  错误：", e$message, "\n")
    return(NULL)
  })
  
  if (is.null(cal_obj)) {
    cat("  跳过时间点", tp, "个月（计算失败）\n")
    next
  }
  
  # ---- 保存为 PDF ----
  pdf_file <- file.path(pdf_dir, paste0("GBM_calibration_external_", tp, "m.pdf"))
  pdf(pdf_file, width = 6, height = 6)
  plot(cal_obj,
       lwd = 2,
       col = colors[as.character(tp)],
       xlab = "Predicted Survival Probability",
       ylab = "Observed Survival Probability",
       xlim = c(0, 1),
       ylim = c(0, 1),
       legend = FALSE,
       conf.int = TRUE)
  abline(0, 1, lty = 2, col = "gray50")
  title(paste("GBM Model - External Validation (", tp, " months)",
              "\n(KM, ", n_groups, " groups, ", n_bootstrap, " bootstraps)"))
  dev.off()
  
  # ---- 保存为 PNG ----
  png_file <- file.path(png_dir, paste0("GBM_calibration_external_", tp, "m.png"))
  png(png_file, width = 6, height = 6, units = "in", res = 300)
  plot(cal_obj,
       lwd = 2,
       col = colors[as.character(tp)],
       xlab = "Predicted Survival Probability",
       ylab = "Observed Survival Probability",
       xlim = c(0, 1),
       ylim = c(0, 1),
       legend = FALSE,
       conf.int = TRUE)
  abline(0, 1, lty = 2, col = "gray50")
  title(paste("GBM Model - External Validation (", tp, " months)",
              "\n(KM, ", n_groups, " groups, ", n_bootstrap, " bootstraps)"))
  dev.off()
  
  cat("  已保存：", pdf_file, "\n")
  cat("  已保存：", png_file, "\n")
  
  # 可选：在屏幕上显示图形（RStudio 中会自动弹出）
  # dev.new()
  # plot(cal_obj, ...)
}

cat("\n===== 全部完成！校准曲线保存在：", out_dir, " =====\n")