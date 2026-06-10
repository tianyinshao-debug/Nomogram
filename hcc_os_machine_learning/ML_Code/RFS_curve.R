# ======================= 外部测试集：GBM 风险分组的 RFS（无复发生存）曲线 =======================
library(survival)
library(ggplot2)
library(survminer)

# 请确保以下对象已存在于全局环境中：
#   gbm_fit      : 已训练好的 GBM 模型
#   best_cutoff  : 基于训练集 OS 确定的最佳风险评分截断值（此处复用于 RFS）
#   test_df      : 外部测试集的数据框，必须包含 rfs（时间）和 rec（复发状态）变量
#   X_test       : 外部测试集的特征矩阵（与训练集特征一致）

if (!exists("gbm_fit")) stop("请先训练 GBM 模型并赋值给 gbm_fit")
if (!exists("best_cutoff")) stop("请先计算最佳截断值 best_cutoff")
if (!exists("test_df") || !exists("X_test")) stop("请准备外部测试集数据 test_df 和特征矩阵 X_test")
if (!"rfs" %in% colnames(test_df) || !"rec" %in% colnames(test_df)) {
  stop("test_df 中必须包含无复发生存时间变量 'rfs' 和复发状态变量 'rec' (0=未复发,1=复发)")
}

# 1. 计算外部测试集的风险评分
risk_test <- predict(gbm_fit, newdata = as.data.frame(X_test),
                     n.trees = gbm_fit$n.trees, type = "link")
risk_test <- as.numeric(risk_test)

# 2. 构建 RFS 数据框并划分风险组
rfs_df <- data.frame(
  risk_score = risk_test,
  time = test_df$rfs,
  status = test_df$rec
)
rfs_df$group <- ifelse(rfs_df$risk_score > best_cutoff, "High risk", "Low risk")
rfs_df$group <- factor(rfs_df$group, levels = c("Low risk", "High risk"))

# 3. 拟合 RFS 的 Kaplan-Meier 曲线
fit_rfs <- survfit(Surv(time, status) ~ group, data = rfs_df)

# 4. 计算 log-rank p 值
sdiff_rfs <- survdiff(Surv(time, status) ~ group, data = rfs_df)
p_val_rfs <- 1 - pchisq(sdiff_rfs$chisq, df = 1)
p_text_rfs <- ifelse(p_val_rfs < 0.0001, "p < 0.0001", paste0("p = ", round(p_val_rfs, 4)))

# 5. 绘制 RFS 的 KM 曲线（含风险表）
p_rfs <- ggsurvplot(fit_rfs,
                    data = rfs_df,
                    pval = p_text_rfs,
                    pval.method = TRUE,
                    conf.int = TRUE,
                    risk.table = TRUE,
                    risk.table.col = "strata",
                    palette = c("#2E86AB", "#A23B72"),
                    xlim = c(0, 80),
                    break.x.by = 10,
                    xlab = "时间（月）",
                    ylab = "无复发生存概率",
                    title = "外部测试集：GBM 风险评分分组的无复发生存曲线",
                    legend.title = "风险组",
                    legend.labs = c("低风险", "高风险"),
                    ggtheme = theme_minimal()
)

# 6. 在 R 图形设备中显示
print(p_rfs)

# 7. 保存为 PDF 和 PNG（可按需修改输出路径）
out_dir <- getwd()  # 或指定输出目录，例如 "./figures"
pdf(file.path(out_dir, "km_curve_RFS_external_test.pdf"), width = 8, height = 7)
print(p_rfs)
dev.off()

png(file.path(out_dir, "km_curve_RFS_external_test.png"), width = 800, height = 700, res = 100)
print(p_rfs)
dev.off()

cat("\n外部测试集的 RFS 生存曲线已生成并保存。\n")