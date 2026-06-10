# ==============================
# 外部验证集亚组分析（按 sex, hepatitis, child_cl, sgm, bclc）
# 计算 C-index 和 IBS（12/36/60个月）
# ==============================
library(readr)
library(survival)
library(gbm)
library(timeROC)

# ------------------------------ 0. 路径与已有对象 ------------------------------
data_dir <- "C:/data/rdata/mlm/"
out_dir <- file.path(data_dir, "ModelResults_FullIBS_3timepoints")

# 加载 GBM 模型
gbm_fit <- readRDS(file.path(out_dir, "gbm_model.rds"))

# 加载外部验证集
test_df <- read_csv(file.path(data_dir, "external_test_processed.csv"), show_col_types = FALSE)

# 检查所需列是否存在
required_cols <- c("os", "suv", "sex", "hepatitis", "child_cl", "sgm", "bclc")
missing_cols <- required_cols[!required_cols %in% names(test_df)]
if (length(missing_cols) > 0) {
  stop("外部验证集缺少以下列：", paste(missing_cols, collapse = ", "))
}

# 确保分类变量为数值型
test_df$sex <- as.numeric(test_df$sex)
test_df$hepatitis <- as.numeric(test_df$hepatitis)
test_df$child_cl <- as.numeric(test_df$child_cl)
test_df$sgm <- as.numeric(test_df$sgm)
test_df$bclc <- as.numeric(test_df$bclc)

# 构建 X_test
lasso_vars <- read_csv(file.path(data_dir, "lasso_selected_lambda.min.csv"), show_col_types = FALSE)
selected_vars <- lasso_vars$variable
X_test <- as.matrix(test_df[, selected_vars])
y_test <- Surv(time = test_df$os, event = test_df$suv)

# ------------------------------ 1. 辅助函数（同上，略） ------------------------------
calc_cindex <- function(pred_risk, y_data) {
  pred_risk <- as.numeric(pred_risk)
  if (length(pred_risk) != nrow(y_data)) return(NA)
  if (length(unique(pred_risk)) < 2) return(0.5)
  result <- tryCatch({
    as.numeric(summary(coxph(y_data ~ pred_risk))$concordance[1])
  }, error = function(e) NA)
  return(result)
}

calc_ibs <- function(surv_prob_matrix, time_points, y_data) {
  if (is.null(surv_prob_matrix) || nrow(surv_prob_matrix) == 0) return(NA)
  max_time <- max(y_data[, "time"], na.rm = TRUE)
  keep_idx <- which(time_points <= max_time)
  if (length(keep_idx) == 0) return(NA)
  if (length(keep_idx) < length(time_points)) {
    time_points <- time_points[keep_idx]
    surv_prob_matrix <- surv_prob_matrix[, keep_idx, drop = FALSE]
  }
  n <- nrow(surv_prob_matrix)
  K <- length(time_points)
  cens_status <- 1 - y_data[, "status"]
  cens_time <- y_data[, "time"]
  cens_fit <- survfit(Surv(cens_time, cens_status) ~ 1)
  get_G <- function(t) {
    idx <- suppressWarnings(max(which(cens_fit$time <= t)))
    if (length(idx) == 0 || is.infinite(idx)) return(1)
    return(cens_fit$surv[idx])
  }
  ibs_sum <- 0
  for (k in 1:K) {
    t <- time_points[k]
    s_hat <- surv_prob_matrix[, k]
    I_gt_t <- as.numeric(y_data[, "time"] > t)
    w <- numeric(n)
    for (i in 1:n) {
      ti <- y_data[i, "time"]
      di <- y_data[i, "status"]
      if (di == 1 && ti <= t) {
        w[i] <- 1 / get_G(ti)
      } else {
        w[i] <- 1 / get_G(t)
      }
    }
    w[!is.finite(w) | is.na(w)] <- 1
    bs_k <- mean(w * (s_hat - I_gt_t)^2, na.rm = TRUE)
    ibs_sum <- ibs_sum + bs_k
  }
  ibs <- ibs_sum / K
  return(ibs)
}

get_surv_prob_from_risk <- function(risk_score, base_haz, train_risk_mean, time_points) {
  risk_offset <- exp(as.numeric(risk_score) - train_risk_mean)
  n_pts <- length(time_points)
  n_samples <- length(risk_score)
  surv_mat <- matrix(1, nrow = n_samples, ncol = n_pts)
  for (i in 1:n_pts) {
    t <- time_points[i]
    idx <- max(which(base_haz$time <= t))
    if (length(idx) == 0 || is.infinite(idx)) {
      cum_haz <- 0
    } else {
      cum_haz <- base_haz$hazard[idx]
    }
    surv_mat[, i] <- exp(-cum_haz * risk_offset)
  }
  return(surv_mat)
}

predict_gbm_full <- function(model, X_data, time_points) {
  X_df <- as.data.frame(X_data)
  colnames(X_df) <- colnames(X_data)
  risk_score <- predict.gbm(model, newdata = X_df, n.trees = model$n.trees, type = "link")
  risk_score <- as.numeric(risk_score)
  surv_mat <- get_surv_prob_from_risk(risk_score, model$base_haz, model$train_risk_mean, time_points)
  return(list(risk = risk_score, surv = surv_mat))
}

subgroup_analysis <- function(data, y, X, model, subgroup_var, subgroup_value, time_pts) {
  idx <- which(data[[subgroup_var]] == subgroup_value)
  if (length(idx) == 0) {
    cat("警告：", subgroup_var, "=", subgroup_value, " 无样本\n")
    return(NULL)
  }
  sub_y <- y[idx, ]
  sub_X <- X[idx, , drop = FALSE]
  
  pred <- predict_gbm_full(model, sub_X, time_pts)
  risk <- pred$risk
  surv_mat <- pred$surv
  
  c_idx <- calc_cindex(risk, sub_y)
  ibs_val <- calc_ibs(surv_mat, time_pts, sub_y)
  
  auc_vals <- sapply(time_pts, function(t) {
    if (t > max(sub_y[, "time"])) return(NA)
    roc_obj <- timeROC(T = sub_y[, "time"], delta = sub_y[, "status"],
                       marker = risk, cause = 1, times = t, iid = FALSE)
    return(roc_obj$AUC[2])
  })
  names(auc_vals) <- paste0("AUC_", time_pts, "m")
  
  result <- c(C_index = c_idx, IBS = ibs_val, auc_vals)
  return(result)
}

# ------------------------------ 2. 执行亚组分析 ------------------------------
time_points <- c(12, 36, 60)
subgroup_vars <- c("sex", "hepatitis", "child_cl", "sgm", "bclc")
results_list <- list()

for (var in subgroup_vars) {
  cat("\n===== 亚组变量:", var, "=====\n")
  levels <- sort(unique(test_df[[var]]))
  for (lev in levels) {
    cat("  水平:", lev, "\n")
    res <- subgroup_analysis(test_df, y_test, X_test, gbm_fit, var, lev, time_points)
    if (!is.null(res)) {
      results_list[[paste(var, lev, sep = "_")]] <- c(Subgroup = var, Level = lev, res)
    }
  }
}

# 转换为数据框，并保留三位小数
results_df <- do.call(rbind, lapply(names(results_list), function(nm) {
  x <- results_list[[nm]]
  data.frame(
    Subgroup = x["Subgroup"],
    Level = x["Level"],
    C_index = round(as.numeric(x["C_index"]), 3),
    IBS = round(as.numeric(x["IBS"]), 3),
    AUC_12m = round(as.numeric(x["AUC_12m"]), 3),
    AUC_36m = round(as.numeric(x["AUC_36m"]), 3),
    AUC_60m = round(as.numeric(x["AUC_60m"]), 3),
    row.names = NULL
  )
}))

# 输出结果
print("===== 亚组分析结果 (外部验证集) =====")
print(results_df)

# 保存 CSV
write.csv(results_df, file.path(out_dir, "subgroup_analysis_Cindex_IBS.csv"), row.names = FALSE)
cat("\n结果已保存至:", file.path(out_dir, "subgroup_analysis_Cindex_IBS.csv"), "\n")