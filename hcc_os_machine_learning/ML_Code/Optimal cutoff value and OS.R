# ======================= 调试版：训练集 KM 曲线 =======================
library(survival)
library(ggplot2)
library(survminer)
library(gbm)

# 路径
data_dir <- "C:/data/rdata/mlm/"
model_path <- file.path(data_dir, "gbm_model.rds")
train_path <- file.path(data_dir, "train_processed.csv")

# 读取数据
train_df <- read.csv(train_path)
cat("训练集维度:", dim(train_df), "\n")

# 读取模型
gbm_fit <- readRDS(model_path)
cat("GBM 模型读取成功，n.trees =", gbm_fit$n.trees, "\n")

# 提取特征（排除 os, suv）
feature_names <- setdiff(names(train_df), c("os", "suv"))
X_train <- train_df[, feature_names, drop = FALSE]
cat("特征数量:", length(feature_names), "\n")

# 预测风险评分
risk_train <- tryCatch({
  predict(gbm_fit, newdata = X_train, n.trees = gbm_fit$n.trees, type = "link")
}, error = function(e) {
  cat("预测出错:", e$message, "\n")
  NULL
})

if (is.null(risk_train)) stop("预测失败")

risk_train <- as.numeric(risk_train)
cat("风险评分范围:", range(risk_train), "\n")

# 计算最佳截断值
scores <- risk_train
n <- length(scores)
min_group_size <- floor(n * 0.2)
best_logrank <- -Inf
best_cutoff <- NA

unique_scores <- unique(scores)
for (cut in unique_scores) {
  group <- ifelse(scores > cut, 1, 0)
  if (sum(group) < min_group_size || sum(1 - group) < min_group_size) next
  sdiff <- survdiff(Surv(train_df$os, train_df$suv) ~ group)
  if (sdiff$chisq > best_logrank) {
    best_logrank <- sdiff$chisq
    best_cutoff <- cut
  }
}
cat("最佳截断值 =", round(best_cutoff, 4), "\n")

# 构建分组
surv_df <- data.frame(
  time = train_df$os,
  status = train_df$suv,
  group = ifelse(risk_train > best_cutoff, "High risk", "Low risk")
)
surv_df$group <- factor(surv_df$group, levels = c("Low risk", "High risk"))
cat("分组样本量:\n")
print(table(surv_df$group))

# 拟合 KM
fit <- survfit(Surv(time, status) ~ group, data = surv_df)

# 计算 p 值
sdiff <- survdiff(Surv(time, status) ~ group, data = surv_df)
p_val <- 1 - pchisq(sdiff$chisq, df = 1)
p_text <- ifelse(p_val < 0.0001, "p < 0.0001", paste0("p = ", round(p_val, 4)))

# 绘图并显式打印
p <- ggsurvplot(fit,
                data = surv_df,
                pval = p_text,
                pval.method = TRUE,
                conf.int = TRUE,
                risk.table = TRUE,
                risk.table.col = "strata",
                palette = c("#2E86AB", "#A23B72"),
                xlim = c(0, 80),
                break.x.by = 10,
                xlab = "时间（月）",
                ylab = "生存概率",
                title = "训练集：GBM 风险评分分组的 Kaplan-Meier 曲线",
                legend.title = "风险组",
                legend.labs = c("低风险", "高风险"),
                ggtheme = theme_minimal())

print(p)   # 关键：强制显示
cat("图形已发送到设备\n")