# ============================== 完整代码：五模型 + 网格搜索 + IBS（时间点12/36/60） ==============================
# 功能：对 RSF、GBM、CoxBoost、弹性网络Cox、XGBoost 进行网格搜索（基于内部验证集），
#       为 RSF、GBM、CoxBoost、EnetCox、XGBoost 生成生存概率曲线并计算 IBS。
# 评估时间点：12, 36, 60 个月
# 输出：性能表格（含 C-index, IBS, AUC_12m, AUC_36m, AUC_60m）、模型对象、分数据集 AUC 曲线图。

library(readr)
library(dplyr)
library(survival)
library(caret)
library(gbm)
library(CoxBoost)
library(glmnet)
library(ranger)
library(timeROC)
library(ggplot2)
library(tidyr)
library(xgboost)        # XGBoost 替代 SVM

# ------------------------------ 1. 路径与设置 ---------------------------
data_dir <- "C:/data/rdata/mlm/"
train_path      <- file.path(data_dir, "train_processed.csv")
internal_path   <- file.path(data_dir, "internal_valid_processed.csv")
external_path   <- file.path(data_dir, "external_test_processed.csv")

selected_lambda <- "lambda.min"
lasso_path <- file.path(data_dir, paste0("lasso_selected_", selected_lambda, ".csv"))

seed_cv <- 2024
out_dir <- file.path(data_dir, "ModelResults_FullIBS_3timepoints")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# ------------------------------ 2. 读取数据 ----------------------------
train_df <- read_csv(train_path, show_col_types = FALSE)
valid_df <- read_csv(internal_path, show_col_types = FALSE)
test_df  <- read_csv(external_path, show_col_types = FALSE)

lasso_vars <- read_csv(lasso_path, show_col_types = FALSE)
selected_vars <- lasso_vars$variable
cat("\n========== 建模变量 (", selected_lambda, ") ==========\n")
cat("变量数量:", length(selected_vars), "\n")
print(selected_vars)

stopifnot(all(selected_vars %in% names(train_df)))
stopifnot(all(c("os", "suv") %in% names(train_df)))

X_train <- as.matrix(train_df[, selected_vars])
X_valid <- as.matrix(valid_df[, selected_vars])
X_test  <- as.matrix(test_df[, selected_vars])

y_train <- Surv(time = train_df$os, event = train_df$suv)
y_valid <- Surv(time = valid_df$os, event = valid_df$suv)
y_test  <- Surv(time = test_df$os,  event = test_df$suv)

# ------------------------------ 3. 公共辅助函数 ---------------------------
# 修改时间点为 12, 36, 60
time_points <- c(12, 36, 60)
cat("时间依赖AUC评估时间点（月）:", time_points, "\n")

# 安全计算 C-index
calc_cindex <- function(pred_risk, y_data) {
  pred_risk <- as.numeric(pred_risk)
  if (length(pred_risk) != nrow(y_data)) return(NA)
  if (length(unique(pred_risk)) < 2) return(0.5)
  result <- tryCatch({
    as.numeric(summary(coxph(y_data ~ pred_risk))$concordance[1])
  }, error = function(e) NA)
  return(result)
}

# 计算 IBS（需要生存概率矩阵）
calc_ibs <- function(surv_prob_matrix, time_points, y_data) {
  if (is.null(surv_prob_matrix) || nrow(surv_prob_matrix) == 0) return(NA)
  
  max_time <- max(y_data[, "time"], na.rm = TRUE)
  keep_idx <- which(time_points <= max_time)
  if (length(keep_idx) == 0) {
    warning("所有时间点均大于最大随访时间 (", max_time, ")，无法计算 IBS")
    return(NA)
  }
  if (length(keep_idx) < length(time_points)) {
    warning("时间点 ", paste(time_points[!time_points %in% time_points[keep_idx]], collapse=","),
            " 超过最大随访时间 ", max_time, "，已忽略")
    time_points <- time_points[keep_idx]
    surv_prob_matrix <- surv_prob_matrix[, keep_idx, drop = FALSE]
  }
  
  n <- nrow(surv_prob_matrix)
  K <- length(time_points)
  
  # 估计删失分布（KM 对删失事件）
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

# 辅助函数：从风险评分和基准风险得到生存概率矩阵
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

# 构建基准风险的函数
build_base_haz <- function(risk_score_train, y_train) {
  risk_score_train <- as.numeric(risk_score_train)
  if (length(risk_score_train) != nrow(y_train)) {
    stop(paste("risk_score_train 长度 (", length(risk_score_train), 
               ") 与 y_train 行数 (", nrow(y_train), ") 不一致"))
  }
  df <- data.frame(risk = risk_score_train,
                   time = y_train[, "time"],
                   status = y_train[, "status"])
  cox_fit <- coxph(Surv(time, status) ~ risk, data = df)
  base_haz <- basehaz(cox_fit, centered = FALSE)
  train_risk_mean <- mean(risk_score_train, na.rm = TRUE)
  return(list(base_haz = base_haz, train_risk_mean = train_risk_mean))
}

# 统一评估函数（RSF、GBM、CoxBoost、EnetCox、XGBoost）
evaluate_model <- function(model, X_data, y_data, model_name, dataset_name, time_pts = time_points,
                           base_haz = NULL, train_risk_mean = NULL, xgb_ntreelimit = NULL) {
  # ---------- 获取风险评分 ----------
  if (inherits(model, "ranger")) {
    X_df <- as.data.frame(X_data)
    colnames(X_df) <- colnames(X_data)
    pred <- predict(model, data = X_df, type = "response")
    surv_mat <- pred$survival
    if (is.null(surv_mat)) stop("RSF预测未返回生存概率矩阵")
    risk_score <- -log(surv_mat[, ncol(surv_mat)])
    surv_time <- pred$unique.death.times
    surv_mat_aligned <- matrix(NA, nrow = nrow(X_data), ncol = length(time_pts))
    for (i in 1:nrow(X_data)) {
      surv_mat_aligned[i, ] <- approx(surv_time, surv_mat[i, ], xout = time_pts,
                                      method = "constant", rule = 2)$y
    }
  } 
  else if (inherits(model, "gbm") || inherits(model, "CoxBoost") || inherits(model, "glmnet")) {
    if (inherits(model, "gbm")) {
      X_df <- as.data.frame(X_data)
      colnames(X_df) <- colnames(X_data)
      n.trees <- ifelse(is.null(model$n.trees), 100, model$n.trees)
      risk_score <- predict(model, newdata = X_df, n.trees = n.trees, type = "link")
    } else if (inherits(model, "CoxBoost")) {
      risk_score <- predict(model, newdata = X_data, type = "lp")
    } else if (inherits(model, "glmnet")) {
      risk_score <- predict(model, newx = X_data, type = "link")
    }
    risk_score <- as.numeric(risk_score)
    if (is.null(base_haz) || is.null(train_risk_mean)) {
      warning("模型 ", model_name, " 缺少基准风险或训练风险均值，无法计算 IBS")
      surv_mat_aligned <- NULL
    } else {
      surv_mat_aligned <- get_surv_prob_from_risk(risk_score, base_haz, train_risk_mean, time_pts)
    }
  } 
  else if (inherits(model, "xgb.Booster")) {
    # XGBoost 预测风险比
    if (is.null(xgb_ntreelimit)) stop("XGBoost 需要提供 ntreelimit (最佳迭代次数)")
    dmat <- xgb.DMatrix(X_data)
    risk_score <- predict(model, dmat, iteration_range = c(1, xgb_ntreelimit))
    if (is.null(base_haz) || is.null(train_risk_mean)) {
      warning("XGBoost 缺少基准风险或训练风险均值，无法计算 IBS")
      surv_mat_aligned <- NULL
    } else {
      surv_mat_aligned <- get_surv_prob_from_risk(risk_score, base_haz, train_risk_mean, time_pts)
    }
  }
  else {
    stop("Unsupported model type")
  }
  
  # C-index
  c_index <- calc_cindex(risk_score, y_data)
  # 时间依赖 AUC
  auc_values <- sapply(time_pts, function(t) {
    max_time <- max(y_data[, "time"], na.rm = TRUE)
    if (t > max_time) return(NA)
    roc_obj <- timeROC(T = y_data[, "time"], delta = y_data[, "status"],
                       marker = risk_score, cause = 1, times = t, iid = FALSE)
    return(roc_obj$AUC[2])
  })
  names(auc_values) <- paste0("AUC_", time_pts, "m")
  # IBS
  ibs <- NA
  if (!is.null(surv_mat_aligned)) {
    ibs <- tryCatch({
      calc_ibs(surv_mat_aligned, time_pts, y_data)
    }, error = function(e) {
      cat("✗ IBS 计算失败 (", dataset_name, "): ", e$message, "\n")
      return(NA)
    })
  }
  result <- c(C_index = c_index, IBS = ibs, auc_values)
  return(result)
}

# =============================== 模型1：RSF (ranger) ===============================
cat("\n[1/5] RSF 网格搜索（基于内部验证集）...\n")
rsf_grid <- expand.grid(
  num.trees = c(300, 500, 800),
  mtry = c(floor(sqrt(length(selected_vars))), floor(length(selected_vars)/3), length(selected_vars)),
  min.node.size = c(5, 10, 20)
)
best_rsf_cindex <- -Inf
best_rsf_params <- NULL
for (i in 1:nrow(rsf_grid)) {
  params <- rsf_grid[i, ]
  cat("  尝试", i, "/", nrow(rsf_grid), ": num.trees=", params$num.trees,
      ", mtry=", params$mtry, ", min.node.size=", params$min.node.size, "\n")
  train_data <- cbind(train_df[, selected_vars], os = train_df$os, suv = train_df$suv)
  fit <- tryCatch({
    ranger(Surv(os, suv) ~ ., data = train_data,
           num.trees = params$num.trees, mtry = params$mtry,
           min.node.size = params$min.node.size,
           write.forest = TRUE, seed = seed_cv)
  }, error = function(e) NULL)
  if (!is.null(fit)) {
    pred <- predict(fit, data = as.data.frame(X_valid), type = "response")$survival
    risk <- -log(pred[, ncol(pred)])
    cidx <- calc_cindex(risk, y_valid)
    if (!is.na(cidx) && cidx > best_rsf_cindex) {
      best_rsf_cindex <- cidx
      best_rsf_params <- params
    }
    cat("    验证集 C-index =", round(cidx, 4), "\n")
  }
}
cat("RSF 最佳参数: num.trees=", best_rsf_params$num.trees,
    ", mtry=", best_rsf_params$mtry,
    ", min.node.size=", best_rsf_params$min.node.size,
    " (验证集 C-index =", round(best_rsf_cindex, 4), ")\n")

train_ranger <- cbind(train_df[, selected_vars], os = train_df$os, suv = train_df$suv)
rsf_fit <- ranger(
  formula = Surv(os, suv) ~ .,
  data = train_ranger,
  num.trees = best_rsf_params$num.trees,
  mtry = best_rsf_params$mtry,
  min.node.size = best_rsf_params$min.node.size,
  importance = "permutation",
  write.forest = TRUE,
  seed = seed_cv
)

# =============================== 模型2：GBM (Cox损失) ===============================
cat("\n[2/5] GBM 网格搜索（基于内部验证集）...\n")
gbm_grid <- expand.grid(
  interaction.depth = c(1, 3, 5),
  shrinkage = c(0.001, 0.01, 0.05),
  n.minobsinnode = c(5, 10),
  n.trees = c(500, 1000)
)
best_gbm_cindex <- -Inf
best_gbm_params <- NULL
for (i in 1:nrow(gbm_grid)) {
  params <- gbm_grid[i, ]
  cat("  尝试", i, "/", nrow(gbm_grid), ": depth=", params$interaction.depth,
      ", shrinkage=", params$shrinkage, ", min.node=", params$n.minobsinnode,
      ", n.trees=", params$n.trees, "\n")
  train_data <- cbind(train_df[, selected_vars], os = train_df$os, suv = train_df$suv)
  fit <- tryCatch({
    gbm(Surv(os, suv) ~ ., data = train_data, distribution = "coxph",
        n.trees = params$n.trees, interaction.depth = params$interaction.depth,
        shrinkage = params$shrinkage, n.minobsinnode = params$n.minobsinnode,
        cv.folds = 0, verbose = FALSE)
  }, error = function(e) NULL)
  if (!is.null(fit)) {
    risk <- predict(fit, newdata = as.data.frame(X_valid),
                    n.trees = params$n.trees, type = "link")
    cidx <- calc_cindex(risk, y_valid)
    if (!is.na(cidx) && cidx > best_gbm_cindex) {
      best_gbm_cindex <- cidx
      best_gbm_params <- params
    }
    cat("    验证集 C-index =", round(cidx, 4), "\n")
  }
}
cat("GBM 最佳参数: interaction.depth=", best_gbm_params$interaction.depth,
    ", shrinkage=", best_gbm_params$shrinkage,
    ", n.minobsinnode=", best_gbm_params$n.minobsinnode,
    ", n.trees=", best_gbm_params$n.trees,
    " (验证集 C-index =", round(best_gbm_cindex, 4), ")\n")

train_gbm <- cbind(train_df[, selected_vars], os = train_df$os, suv = train_df$suv)
gbm_fit <- gbm(
  formula = Surv(os, suv) ~ .,
  data = train_gbm,
  distribution = "coxph",
  n.trees = best_gbm_params$n.trees,
  interaction.depth = best_gbm_params$interaction.depth,
  shrinkage = best_gbm_params$shrinkage,
  n.minobsinnode = best_gbm_params$n.minobsinnode,
  cv.folds = 0,
  verbose = FALSE
)
gbm_fit$n.trees <- best_gbm_params$n.trees
risk_gbm_train <- as.numeric(predict(gbm_fit, newdata = as.data.frame(X_train), 
                                     n.trees = gbm_fit$n.trees, type = "link"))
gbm_base <- build_base_haz(risk_gbm_train, y_train)
gbm_fit$base_haz <- gbm_base$base_haz
gbm_fit$train_risk_mean <- gbm_base$train_risk_mean

# =============================== 模型3：CoxBoost ===============================
cat("\n[3/5] CoxBoost 网格搜索（基于内部验证集）...\n")
stepno_candidates <- c(50, 100, 200)
penalty_candidates <- c(0.1, 0.5, 1) * sqrt(ncol(X_train))
best_cb_cindex <- -Inf
best_cb_stepno <- NULL
best_cb_penalty <- NULL
for (step in stepno_candidates) {
  for (pen in penalty_candidates) {
    cat("  尝试 stepno =", step, ", penalty =", pen, "\n")
    fit <- tryCatch({
      CoxBoost(time = train_df$os, status = train_df$suv,
               x = X_train, stepno = step, penalty = pen,
               standardize = TRUE)
    }, error = function(e) NULL)
    if (!is.null(fit)) {
      risk <- predict(fit, newdata = X_valid, type = "lp")
      cidx <- calc_cindex(risk, y_valid)
      if (!is.na(cidx) && cidx > best_cb_cindex) {
        best_cb_cindex <- cidx
        best_cb_stepno <- step
        best_cb_penalty <- pen
      }
      cat("    验证集 C-index =", round(cidx, 4), "\n")
    }
  }
}
cat("CoxBoost 最佳参数: stepno =", best_cb_stepno, ", penalty =", best_cb_penalty,
    " (验证集 C-index =", round(best_cb_cindex, 4), ")\n")

coxboost_fit <- CoxBoost(
  time = train_df$os,
  status = train_df$suv,
  x = X_train,
  stepno = best_cb_stepno,
  penalty = best_cb_penalty,
  standardize = TRUE
)
risk_cb_train <- as.numeric(predict(coxboost_fit, newdata = X_train, type = "lp"))
cb_base <- build_base_haz(risk_cb_train, y_train)
coxboost_fit$base_haz <- cb_base$base_haz
coxboost_fit$train_risk_mean <- cb_base$train_risk_mean

# =============================== 模型4：弹性网络Cox ===============================
cat("\n[4/5] 弹性网络Cox 网格搜索（基于内部验证集）...\n")
alpha_candidates <- seq(0.1, 0.9, by = 0.2)
best_enet_cindex <- -Inf
best_alpha <- NULL
best_lambda <- NULL
for (a in alpha_candidates) {
  cat("  尝试 alpha =", a, "\n")
  fit_cv <- tryCatch({
    cv.glmnet(x = X_train, y = y_train, family = "cox",
              alpha = a, nfolds = 3)
  }, error = function(e) NULL)
  if (!is.null(fit_cv)) {
    lambda_opt <- fit_cv$lambda.min
    fit <- glmnet(x = X_train, y = y_train, family = "cox", alpha = a, lambda = lambda_opt)
    risk <- predict(fit, newx = X_valid, type = "link")
    cidx <- calc_cindex(risk, y_valid)
    if (!is.na(cidx) && cidx > best_enet_cindex) {
      best_enet_cindex <- cidx
      best_alpha <- a
      best_lambda <- lambda_opt
    }
    cat("    验证集 C-index =", round(cidx, 4), "\n")
  }
}
cat("弹性网络最佳 alpha =", best_alpha, ", lambda =", best_lambda,
    " (验证集 C-index =", round(best_enet_cindex, 4), ")\n")

enet_fit <- glmnet(
  x = X_train,
  y = y_train,
  family = "cox",
  alpha = best_alpha,
  lambda = best_lambda
)
risk_enet_train <- as.numeric(predict(enet_fit, newx = X_train, type = "link"))
enet_base <- build_base_haz(risk_enet_train, y_train)
enet_fit$base_haz <- enet_base$base_haz
enet_fit$train_risk_mean <- enet_base$train_risk_mean

# =============================== 模型5：XGBoost (Cox损失) ===============================
cat("\n[5/5] XGBoost 网格搜索（基于内部验证集）...\n")

# 准备标签（XGBoost survival:cox 要求：事件时间正，删失时间负）
xgb_label_train <- ifelse(train_df$suv == 1, train_df$os, -train_df$os)
xgb_label_valid <- ifelse(valid_df$suv == 1, valid_df$os, -valid_df$os)

dtrain <- xgb.DMatrix(data = X_train, label = xgb_label_train)
dvalid <- xgb.DMatrix(data = X_valid, label = xgb_label_valid)

# 定义网格（参考其他模型，适度调整）
xgb_grid <- expand.grid(
  nrounds = c(100, 200, 300),
  max_depth = c(2, 3, 4),
  eta = c(0.01, 0.05, 0.1),
  subsample = c(0.6, 0.8, 1.0),
  colsample_bytree = c(0.6, 0.8, 1.0)
)

best_xgb_cindex <- -Inf
best_xgb_fit <- NULL
best_xgb_params <- list()
best_xgb_ntrees <- NULL

base_params <- list(
  objective = "survival:cox",
  eval_metric = "cox-nloglik",
  booster = "gbtree"
)

for (i in 1:nrow(xgb_grid)) {
  params <- base_params
  params$max_depth <- xgb_grid[i, "max_depth"]
  params$eta <- xgb_grid[i, "eta"]
  params$subsample <- xgb_grid[i, "subsample"]
  params$colsample_bytree <- xgb_grid[i, "colsample_bytree"]
  nrounds_curr <- xgb_grid[i, "nrounds"]
  
  cat("  尝试", i, "/", nrow(xgb_grid), ": max_depth=", params$max_depth,
      ", eta=", params$eta, ", subsample=", params$subsample,
      ", colsample_bytree=", params$colsample_bytree,
      ", nrounds=", nrounds_curr, "\n")
  
  set.seed(seed_cv)
  fit <- tryCatch({
    xgb.train(
      params = params,
      data = dtrain,
      nrounds = nrounds_curr,
      evals = list(valid = dvalid),
      early_stopping_rounds = 20,
      verbose = 0,
      maximize = FALSE
    )
  }, error = function(e) NULL)
  
  if (!is.null(fit)) {
    best_iter <- fit$best_iteration
    if (is.null(best_iter) || best_iter == 0) best_iter <- nrounds_curr
    risk_valid <- predict(fit, dvalid, iteration_range = c(1, best_iter))
    cidx <- calc_cindex(risk_valid, y_valid)
    
    if (!is.na(cidx) && cidx > best_xgb_cindex) {
      best_xgb_cindex <- cidx
      best_xgb_fit <- fit
      best_xgb_params <- list(
        max_depth = params$max_depth,
        eta = params$eta,
        subsample = params$subsample,
        colsample_bytree = params$colsample_bytree
      )
      best_xgb_ntrees <- best_iter
    }
    cat("    验证集 C-index =", round(cidx, 4), "\n")
  }
}

cat("XGBoost 最佳参数: max_depth=", best_xgb_params$max_depth,
    ", eta=", best_xgb_params$eta,
    ", subsample=", best_xgb_params$subsample,
    ", colsample_bytree=", best_xgb_params$colsample_bytree,
    ", nrounds=", best_xgb_ntrees,
    " (验证集 C-index =", round(best_xgb_cindex, 4), ")\n")

# 使用最佳模型重新训练（已经是最佳拟合的模型，直接使用）
# 计算基准风险（基于训练集）
xgb_train_risk <- predict(best_xgb_fit, dtrain, iteration_range = c(1, best_xgb_ntrees))
xgb_base <- build_base_haz(xgb_train_risk, y_train)

# 单独存储基准风险信息（不修改模型对象）
xgb_base_haz <- xgb_base$base_haz
xgb_train_risk_mean <- xgb_base$train_risk_mean
xgb_ntreelimit <- best_xgb_ntrees

# =============================== 最终模型评估 ===============================
cat("\n========== 最终模型评估 ==========\n")

# RSF
for (dt_name in c("train", "valid", "test")) {
  X_data <- get(paste0("X_", dt_name))
  y_data <- get(paste0("y_", dt_name))
  eval_res <- evaluate_model(rsf_fit, X_data, y_data, "RSF", dt_name, time_points)
  assign(paste0("eval_rsf_", dt_name), eval_res)
  cat("RSF", dt_name, "集结果:\n"); print(round(eval_res, 4)); cat("\n")
}
# GBM
for (dt_name in c("train", "valid", "test")) {
  X_data <- get(paste0("X_", dt_name))
  y_data <- get(paste0("y_", dt_name))
  eval_res <- evaluate_model(gbm_fit, X_data, y_data, "GBM", dt_name, time_points,
                             base_haz = gbm_fit$base_haz, train_risk_mean = gbm_fit$train_risk_mean)
  assign(paste0("eval_gbm_", dt_name), eval_res)
  cat("GBM", dt_name, "集结果:\n"); print(round(eval_res, 4)); cat("\n")
}
# CoxBoost
for (dt_name in c("train", "valid", "test")) {
  X_data <- get(paste0("X_", dt_name))
  y_data <- get(paste0("y_", dt_name))
  eval_res <- evaluate_model(coxboost_fit, X_data, y_data, "CoxBoost", dt_name, time_points,
                             base_haz = coxboost_fit$base_haz, train_risk_mean = coxboost_fit$train_risk_mean)
  assign(paste0("eval_coxboost_", dt_name), eval_res)
  cat("CoxBoost", dt_name, "集结果:\n"); print(round(eval_res, 4)); cat("\n")
}
# EnetCox
for (dt_name in c("train", "valid", "test")) {
  X_data <- get(paste0("X_", dt_name))
  y_data <- get(paste0("y_", dt_name))
  eval_res <- evaluate_model(enet_fit, X_data, y_data, "EnetCox", dt_name, time_points,
                             base_haz = enet_fit$base_haz, train_risk_mean = enet_fit$train_risk_mean)
  assign(paste0("eval_enetcox_", dt_name), eval_res)
  cat("EnetCox", dt_name, "集结果:\n"); print(round(eval_res, 4)); cat("\n")
}
# XGBoost
for (dt_name in c("train", "valid", "test")) {
  X_data <- get(paste0("X_", dt_name))
  y_data <- get(paste0("y_", dt_name))
  eval_res <- evaluate_model(best_xgb_fit, X_data, y_data, "XGBoost", dt_name, time_points,
                             base_haz = xgb_base_haz, 
                             train_risk_mean = xgb_train_risk_mean,
                             xgb_ntreelimit = xgb_ntreelimit)
  assign(paste0("eval_xgb_", dt_name), eval_res)
  cat("XGBoost", dt_name, "集结果:\n"); print(round(eval_res, 4)); cat("\n")
}

# ------------------------------ 汇总结果 -------------------------
results_list <- list()
model_names_short <- c("RSF", "GBM", "CoxBoost", "EnetCox", "XGBoost")
prefixes <- c("rsf", "gbm", "coxboost", "enetcox", "xgb")
for (i in seq_along(model_names_short)) {
  model_show <- model_names_short[i]
  prefix <- prefixes[i]
  for (dataset in c("train", "valid", "test")) {
    eval_name <- paste0("eval_", prefix, "_", dataset)
    if (exists(eval_name)) {
      results_list[[paste(model_show, dataset, sep = "_")]] <- get(eval_name)
    }
  }
}

results_df <- do.call(rbind, lapply(names(results_list), function(n) {
  parts <- strsplit(n, "_")[[1]]
  model <- parts[1]
  dataset <- parts[2]
  values <- results_list[[n]]
  data.frame(Model = model, Dataset = dataset, t(values), row.names = NULL)
}))
rownames(results_df) <- NULL
col_order <- c("Model", "Dataset", "C_index", "IBS", grep("AUC_", colnames(results_df), value = TRUE))
results_df <- results_df[, col_order]
print("===== 五模型最终性能汇总（时间点12/36/60）=====")
print(results_df)

write.csv(results_df, file = file.path(out_dir, "five_models_performance_3timepoints.csv"), row.names = FALSE)

# 保存模型对象
saveRDS(rsf_fit, file = file.path(out_dir, "rsf_model.rds"))
saveRDS(gbm_fit, file = file.path(out_dir, "gbm_model.rds"))
saveRDS(coxboost_fit, file = file.path(out_dir, "coxboost_model.rds"))
saveRDS(enet_fit, file = file.path(out_dir, "enet_model.rds"))
saveRDS(best_xgb_fit, file = file.path(out_dir, "xgb_model.rds"))

# ------------------------------ AUC 可视化 -------------------------
plot_df <- results_df %>%
  pivot_longer(cols = starts_with("AUC_"), names_to = "TimePoint", values_to = "AUC") %>%
  mutate(Time = as.numeric(gsub("AUC_(\\d+)m", "\\1", TimePoint)),
         Dataset = factor(Dataset, levels = c("train", "valid", "test")),
         Model = factor(Model, levels = c("RSF", "GBM", "CoxBoost", "EnetCox", "XGBoost")))

model_colors <- c("RSF" = "#E41A1C", "GBM" = "#377EB8", "CoxBoost" = "#4DAF4A",
                  "EnetCox" = "#984EA3", "XGBoost" = "#FF7F00")

for (dt in c("train", "valid", "test")) {
  dt_name <- switch(dt, train = "训练集", valid = "内部验证集", test = "外部测试集")
  p <- plot_df %>%
    filter(Dataset == dt) %>%
    ggplot(aes(x = Time, y = AUC, color = Model)) +
    geom_line(linewidth = 1.2) + geom_point(size = 3) +
    scale_x_continuous(breaks = time_points) +
    scale_color_manual(values = model_colors) +
    coord_cartesian(ylim = c(0, 1)) +
    labs(title = paste0("时间依赖AUC比较 - ", dt_name, " (12/36/60个月)"),
         x = "时间（月）", y = "AUC") +
    theme_minimal() + theme(legend.position = "bottom")
  ggsave(file.path(out_dir, paste0("time_auc_", dt, ".pdf")), p, width = 6, height = 5)
  ggsave(file.path(out_dir, paste0("time_auc_", dt, ".png")), p, width = 6, height = 5, dpi = 300)
  cat("已保存:", dt_name, "AUC图\n")
}

cat("\n全部完成！结果保存在:", out_dir, "\n")
cat("模型性能表格: five_models_performance_3timepoints.csv\n")
cat("AUC分数据集图: time_auc_train/valid/test.pdf/png\n")