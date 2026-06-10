# ======================= 模型比较：GBM vs 传统评分（外部验证集） =======================
# 传统评分：non3（三分类）, mat16.3（二分类）, cupi_cl（二分类）
library(survival)
library(boot)

# 假设外部验证集数据框 test_df 包含以下列：
#   os      : 生存时间（月）
#   suv     : 生存状态（0=删失，1=死亡）
#   non3    : 三分类变量（1,2,3 或因子）
#   mat16.3 : 二分类变量（0/1）
#   cupi_cl : 二分类变量（0/1）
# 同时，已有 gbm_fit 对象，以及 X_test 特征矩阵（用于 GBM 预测）

# 1. 计算 GBM 模型在外部验证集上的风险评分（线性预测）
risk_gbm <- predict.gbm(gbm_fit, newdata = as.data.frame(X_test),
                        n.trees = gbm_fit$n.trees, type = "link")
risk_gbm <- as.numeric(risk_gbm)

# 2. 拟合传统评分 Cox 模型，提取线性预测
cox_non3  <- coxph(Surv(os, suv) ~ non3,    data = test_df)
cox_mat16 <- coxph(Surv(os, suv) ~ mat16.3, data = test_df)
cox_cupi  <- coxph(Surv(os, suv) ~ cupi_cl, data = test_df)

risk_non3  <- predict(cox_non3,  type = "lp")
risk_mat16 <- predict(cox_mat16, type = "lp")
risk_cupi  <- predict(cox_cupi,  type = "lp")

# 3. 定义计算 C‑index 的函数（用于 bootstrap）
c_index <- function(data, indices, pred) {
  d <- data[indices, ]
  c_index_val <- as.numeric(summary(coxph(Surv(os, suv) ~ pred, data = d))$concordance[1])
  return(c_index_val)
}

# 4. Bootstrap 函数：对每个模型计算 C‑index 及其置信区间，同时存储差异
B <- 500
set.seed(2024)

# 存储 bootstrap 结果
c_gbm_boot <- numeric(B)
c_non3_boot <- numeric(B)
c_mat16_boot <- numeric(B)
c_cupi_boot <- numeric(B)
diff_gbm_non3 <- numeric(B)
diff_gbm_mat16 <- numeric(B)
diff_gbm_cupi <- numeric(B)

for (b in 1:B) {
  idx <- sample(1:nrow(test_df), size = nrow(test_df), replace = TRUE)
  d <- test_df[idx, ]
  
  # GBM 风险（对应相同的 idx）
  risk_gbm_b <- risk_gbm[idx]
  c_gbm_boot[b] <- as.numeric(summary(coxph(Surv(os, suv) ~ risk_gbm_b, data = d))$concordance[1])
  
  # 传统评分（直接从原预测向量中取子集）
  risk_non3_b <- risk_non3[idx]
  c_non3_boot[b] <- as.numeric(summary(coxph(Surv(os, suv) ~ risk_non3_b, data = d))$concordance[1])
  
  risk_mat16_b <- risk_mat16[idx]
  c_mat16_boot[b] <- as.numeric(summary(coxph(Surv(os, suv) ~ risk_mat16_b, data = d))$concordance[1])
  
  risk_cupi_b <- risk_cupi[idx]
  c_cupi_boot[b] <- as.numeric(summary(coxph(Surv(os, suv) ~ risk_cupi_b, data = d))$concordance[1])
  
  # 差异
  diff_gbm_non3[b] <- c_gbm_boot[b] - c_non3_boot[b]
  diff_gbm_mat16[b] <- c_gbm_boot[b] - c_mat16_boot[b]
  diff_gbm_cupi[b] <- c_gbm_boot[b] - c_cupi_boot[b]
}

# 5. 计算各模型 C‑index 的点估计（基于全样本）和 Bootstrap 置信区间
c_gbm_est <- as.numeric(summary(coxph(Surv(os, suv) ~ risk_gbm, data = test_df))$concordance[1])
c_non3_est <- as.numeric(summary(coxph(Surv(os, suv) ~ risk_non3, data = test_df))$concordance[1])
c_mat16_est <- as.numeric(summary(coxph(Surv(os, suv) ~ risk_mat16, data = test_df))$concordance[1])
c_cupi_est <- as.numeric(summary(coxph(Surv(os, suv) ~ risk_cupi, data = test_df))$concordance[1])

ci_gbm <- quantile(c_gbm_boot, probs = c(0.025, 0.975), na.rm = TRUE)
ci_non3 <- quantile(c_non3_boot, probs = c(0.025, 0.975), na.rm = TRUE)
ci_mat16 <- quantile(c_mat16_boot, probs = c(0.025, 0.975), na.rm = TRUE)
ci_cupi <- quantile(c_cupi_boot, probs = c(0.025, 0.975), na.rm = TRUE)

# 6. 计算差异的 p 值（双侧，基于 Bootstrap 分布中 0 的位置）
p_gbm_non3 <- 2 * min(mean(diff_gbm_non3 > 0, na.rm = TRUE), mean(diff_gbm_non3 < 0, na.rm = TRUE))
p_gbm_mat16 <- 2 * min(mean(diff_gbm_mat16 > 0, na.rm = TRUE), mean(diff_gbm_mat16 < 0, na.rm = TRUE))
p_gbm_cupi <- 2 * min(mean(diff_gbm_cupi > 0, na.rm = TRUE), mean(diff_gbm_cupi < 0, na.rm = TRUE))

# 7. 汇总结果表格
result_table <- data.frame(
  Model = c("GBM", "non3", "mat16.3", "cupi_cl"),
  C_index = round(c(c_gbm_est, c_non3_est, c_mat16_est, c_cupi_est), 3),
  CI_lower = round(c(ci_gbm[1], ci_non3[1], ci_mat16[1], ci_cupi[1]), 3),
  CI_upper = round(c(ci_gbm[2], ci_non3[2], ci_mat16[2], ci_cupi[2]), 3)
)

print("===== 模型 C‑index 比较（外部验证集）=====")
print(result_table)

cat("\n===== GBM vs 传统模型差异的 Bootstrap p 值 =====\n")
cat(sprintf("GBM vs non3       : p = %.4f\n", p_gbm_non3))
cat(sprintf("GBM vs mat16.3    : p = %.4f\n", p_gbm_mat16))
cat(sprintf("GBM vs cupi_cl    : p = %.4f\n", p_gbm_cupi))

# 可选：保存结果
# write.csv(result_table, file.path(out_dir, "model_comparison.csv"), row.names = FALSE)