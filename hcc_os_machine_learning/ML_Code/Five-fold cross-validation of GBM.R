# ======================= 外部测试集 5 折交叉验证（GBM 最佳模型） =======================
# 输出每折的 C-index、IBS、AUC_12m、AUC_36m、AUC_60m，不汇总平均值和标准差

# 确保 time_points 已定义
time_points <- c(12, 36, 60)

# 创建外部测试集的 5 折索引
set.seed(seed_cv)
test_folds <- createFolds(1:nrow(X_test), k = 5, list = TRUE)

# 初始化结果数据框
cv_results <- data.frame(Fold = 1:5,
                         C_index = NA,
                         IBS = NA,
                         AUC_12m = NA,
                         AUC_36m = NA,
                         AUC_60m = NA)

for (fold in 1:5) {
  val_idx <- test_folds[[fold]]
  train_idx <- setdiff(1:nrow(X_test), val_idx)
  
  X_cv_train <- X_test[train_idx, ]
  y_cv_train <- y_test[train_idx]
  X_cv_val <- X_test[val_idx, ]
  y_cv_val <- y_test[val_idx]
  
  # 在当前训练折上拟合 GBM 模型（使用最佳超参数）
  train_data <- as.data.frame(X_cv_train)
  colnames(train_data) <- colnames(X_test)
  train_data$os <- y_cv_train[, "time"]
  train_data$suv <- y_cv_train[, "status"]
  
  fit <- gbm(
    formula = Surv(os, suv) ~ .,
    data = train_data,
    distribution = "coxph",
    n.trees = best_gbm_params$n.trees,
    interaction.depth = best_gbm_params$interaction.depth,
    shrinkage = best_gbm_params$shrinkage,
    n.minobsinnode = best_gbm_params$n.minobsinnode,
    cv.folds = 0,
    verbose = FALSE
  )
  
  # 计算训练折的风险评分，用于构建基准风险
  risk_train <- predict(fit, newdata = train_data,
                        n.trees = best_gbm_params$n.trees, type = "link")
  base <- build_base_haz(risk_train, y_cv_train)
  
  # 在验证折上预测风险评分和生存概率
  val_data <- as.data.frame(X_cv_val)
  colnames(val_data) <- colnames(X_test)
  risk_val <- predict(fit, newdata = val_data,
                      n.trees = best_gbm_params$n.trees, type = "link")
  surv_mat <- get_surv_prob_from_risk(risk_val, base$base_haz,
                                      base$train_risk_mean, time_points)
  
  # 计算 C-index
  c_idx <- calc_cindex(risk_val, y_cv_val)
  
  # 计算 IBS
  ibs_val <- calc_ibs(surv_mat, time_points, y_cv_val)
  
  # 计算时间依赖 AUC
  auc_12 <- tryCatch({
    roc12 <- timeROC(T = y_cv_val[, "time"], delta = y_cv_val[, "status"],
                     marker = risk_val, cause = 1, times = 12, iid = FALSE)
    roc12$AUC[2]
  }, error = function(e) NA)
  auc_36 <- tryCatch({
    roc36 <- timeROC(T = y_cv_val[, "time"], delta = y_cv_val[, "status"],
                     marker = risk_val, cause = 1, times = 36, iid = FALSE)
    roc36$AUC[2]
  }, error = function(e) NA)
  auc_60 <- tryCatch({
    roc60 <- timeROC(T = y_cv_val[, "time"], delta = y_cv_val[, "status"],
                     marker = risk_val, cause = 1, times = 60, iid = FALSE)
    roc60$AUC[2]
  }, error = function(e) NA)
  
  cv_results[fold, ] <- c(fold, c_idx, ibs_val, auc_12, auc_36, auc_60)
  cat(sprintf("折 %d: C-index=%.4f, IBS=%.4f, AUC_12m=%.4f, AUC_36m=%.4f, AUC_60m=%.4f\n",
              fold, c_idx, ibs_val, auc_12, auc_36, auc_60))
}

# 输出完整结果表格
print(cv_results)

# 保存结果（可选）
write.csv(cv_results, file = file.path(out_dir, "gbm_5fold_cv_external.csv"), row.names = FALSE)
cat("\n详细结果已保存至:", file.path(out_dir, "gbm_5fold_cv_external.csv"))