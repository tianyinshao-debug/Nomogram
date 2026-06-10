# ==============================
# GBM SHAP 条形图（前8个变量，全训练集，背景集=100）
# ==============================
library(fastshap)
library(shapviz)
library(gbm)
library(ggplot2)
library(dplyr)

# ------------------------------ 1. 加载模型和数据 ------------------------------
data_dir <- "C:/data/rdata/mlm/"
out_dir <- file.path(data_dir, "ModelResults_FullIBS_3timepoints")

gbm_fit <- readRDS(file.path(out_dir, "gbm_model.rds"))
train_df <- read.csv(file.path(data_dir, "train_processed.csv"))
lasso_vars <- read.csv(file.path(data_dir, "lasso_selected_lambda.min.csv"))
selected_vars <- lasso_vars$variable
X_train <- as.matrix(train_df[, selected_vars])

# ------------------------------ 2. 准备数据（全样本，背景100） ------------------------------
set.seed(2024)
X_sample <- X_train                        # 全量训练集
bg_size <- 100                             # 背景集大小 = 100
bg_idx <- sample(nrow(X_train), bg_size)
X_bg <- X_train[bg_idx, , drop = FALSE]

# ------------------------------ 3. GBM 预测包装器 ------------------------------
pred_gbm <- function(object, newdata) {
  newdata <- as.data.frame(newdata)
  colnames(newdata) <- colnames(X_train)
  lp <- predict(object, newdata = newdata, n.trees = object$n.trees, type = "link")
  matrix(lp, ncol = 1)   # 必须返回列矩阵
}

# ------------------------------ 4. 计算 SHAP 矩阵 ------------------------------
cat("Calculating SHAP (full training set, nsim=25, bg=100) ...\n")
shap_gbm <- fastshap::explain(
  gbm_fit,
  X = X_sample,
  nsim = 25,
  pred_wrapper = pred_gbm,
  bg_X = X_bg
)

# 处理列表格式（兼容不同 fastshap 版本）
if (is.list(shap_gbm) && "shap_vals" %in% names(shap_gbm)) {
  shap_gbm <- shap_gbm$shap_vals
}
shap_gbm <- as.matrix(shap_gbm)

# 行数对齐（防止因版本差异导致行数不一致）
if (nrow(shap_gbm) != nrow(X_sample)) {
  n <- min(nrow(shap_gbm), nrow(X_sample))
  shap_gbm <- shap_gbm[1:n, , drop = FALSE]
  X_sample <- X_sample[1:n, , drop = FALSE]
  warning("行数不匹配，已截取至 ", n, " 行")
}

# ------------------------------ 5. 计算平均绝对 SHAP 并取前8 ------------------------------
mean_abs_shap <- colMeans(abs(shap_gbm))
imp_df <- data.frame(
  feature = names(mean_abs_shap),
  importance = mean_abs_shap
) %>%
  arrange(desc(importance)) %>%
  slice(1:8) %>%                           # 取前8个
  mutate(feature = factor(feature, levels = rev(feature)))  # 降序排列

# ------------------------------ 6. 绘制条形图（无数值标签） ------------------------------
p <- ggplot(imp_df, aes(x = importance, y = feature)) +
  geom_col(fill = "#1F77B4", width = 0.7) +
  labs(x = "Mean |SHAP|", y = NULL,
       title = "GBM Feature Importance (SHAP, Top 8)",
       subtitle = "Full training set, bg=100, nsim=25") +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5, color = "gray30"),
    axis.text.y = element_text(size = 11, angle = 0, hjust = 1),
    panel.grid.major.y = element_blank()
  ) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.05)))

# 保存图片
ggsave(file.path(out_dir, "GBM_shap_barplot_top8_bg100.pdf"), p, width = 8, height = 5)
ggsave(file.path(out_dir, "GBM_shap_barplot_top8_bg100.png"), p, width = 8, height = 5, dpi = 300)

print(p)
cat("SHAP 条形图（前8个变量，背景100）已保存至:", out_dir, "\n")

# ------------------------------ 7. 生成 SHAP 蜂群图（前8个变量） ------------------------------
shp <- shapviz(shap_gbm, X = X_sample)
p_beeswarm <- sv_importance(shp, kind = "beeswarm", max_display = 8) +
  ggtitle("GBM SHAP Summary Plot (Top 8, Full Training Set)") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

ggsave(file.path(out_dir, "GBM_shap_beeswarm_top8.pdf"), p_beeswarm, width = 10, height = 6)
ggsave(file.path(out_dir, "GBM_shap_beeswarm_top8.png"), p_beeswarm, width = 10, height = 6, dpi = 300)

print(p_beeswarm)
cat("SHAP 蜂群图（前8个变量）已保存至:", out_dir, "\n")