# 加载必要的包
library(readxl)
library(caret)
library(pROC)
library(randomForest)
library(xgboost)
library(e1071)
library(nnet)
library(ggplot2)
library(patchwork)
library(dplyr)
library(recipes)

# 设置随机种子确保结果可重复
set.seed(123)

# 1. 读取数据
train_data <- read_excel("C:/data/lyq0118.xlsx")
valid_data <- read_excel("C:/data/sty0118.xlsx")

# 2. 数据预处理
# 首先查看数据结构
cat("训练集变量类型:\n")
str(train_data)
cat("\n验证集变量类型:\n")
str(valid_data)

# 转换变量类型并确保因子水平是有效的R变量名
train_data$vt <- as.factor(train_data$vt)
train_data$child <- as.factor(train_data$child)
train_data$hrev <- as.factor(train_data$hrev)

valid_data$vt <- as.factor(valid_data$vt)
valid_data$child <- as.factor(valid_data$child)
valid_data$hrev <- as.factor(valid_data$hrev)

# 关键修复：确保因子水平是有效的R变量名
if (all(levels(train_data$hrev) %in% c("0", "1"))) {
  levels(train_data$hrev) <- c("Negative", "Positive")
  levels(valid_data$hrev) <- c("Negative", "Positive")
} else {
  levels(train_data$hrev) <- make.names(levels(train_data$hrev))
  levels(valid_data$hrev) <- make.names(levels(valid_data$hrev))
}

# 对其他分类变量也做同样处理
levels(train_data$vt) <- make.names(levels(train_data$vt))
levels(valid_data$vt) <- make.names(levels(valid_data$vt))
levels(train_data$child) <- make.names(levels(train_data$child))
levels(valid_data$child) <- make.names(levels(valid_data$child))

cat("\n修复后的因子水平:\n")
cat("hrev - 训练集:", levels(train_data$hrev), "\n")
cat("hrev - 验证集:", levels(valid_data$hrev), "\n")
cat("vt - 训练集:", levels(train_data$vt), "\n")
cat("child - 训练集:", levels(train_data$child), "\n")

# 3. 数据预处理：标准化连续变量 + 独热编码分类变量
cat("\n=== 数据预处理 ===\n")

# 定义变量类型
continuous_vars <- c("sgd", "spd", "wzd", "spc", "spk", "sph")
categorical_vars <- c("vt", "child")
outcome_var <- "hrev"

# 创建预处理配方（使用recipes包）
prep_recipe <- recipe(as.formula(paste(outcome_var, "~", paste(c(continuous_vars, categorical_vars), collapse = "+"))),
                      data = train_data) %>%
  # 标准化连续变量
  step_normalize(all_of(continuous_vars)) %>%
  # 独热编码分类变量
  step_dummy(all_of(categorical_vars), one_hot = TRUE)

# 训练预处理步骤
prep_trained <- prep(prep_recipe, training = train_data)

# 应用预处理到训练集和验证集
train_processed <- bake(prep_trained, new_data = train_data)
valid_processed <- bake(prep_trained, new_data = valid_data)

# 查看预处理后的数据结构
cat("\n预处理后的训练集维度:", dim(train_processed), "\n")
cat("预处理后的验证集维度:", dim(valid_processed), "\n")
cat("\n预处理后的训练集变量名:\n")
print(names(train_processed))

# 4. 准备建模数据
# 获取特征列名（排除结局变量）
feature_names <- setdiff(names(train_processed), outcome_var)

cat("\n特征数量:", length(feature_names), "\n")
cat("特征名称:\n")
print(feature_names)

# 5. 创建训练控制参数（用于5折交叉验证）
train_control <- trainControl(
  method = "cv",
  number = 5,
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  savePredictions = "final",
  verboseIter = FALSE
)

# 6. 构建模型函数
build_models <- function(train_data, valid_data, feature_names, outcome_var) {
  models <- list()
  predictions <- list(train = list(), valid = list())
  probabilities <- list(train = list(), valid = list())
  
  # 准备公式
  formula <- as.formula(paste(outcome_var, "~", paste(feature_names, collapse = "+")))
  
  # 获取数值型结局变量
  train_y_numeric <- ifelse(train_data[[outcome_var]] == "Positive", 1, 0)
  valid_y_numeric <- ifelse(valid_data[[outcome_var]] == "Positive", 1, 0)
  
  # 1) 随机森林 (RF)
  cat("\n1. 训练随机森林模型...\n")
  tryCatch({
    models$rf <- train(
      formula,
      data = train_data,
      method = "rf",
      trControl = train_control,
      metric = "ROC",
      tuneLength = 3,
      ntree = 100,
      importance = TRUE
    )
    cat("  随机森林训练完成\n")
    
    # 训练集预测（使用5折交叉验证的预测）
    rf_train_prob <- models$rf$pred[models$rf$pred$mtry == models$rf$bestTune$mtry, "Positive"]
    
    # 确保预测顺序与原始数据一致
    rf_train_prob <- rf_train_prob[order(models$rf$pred[models$rf$pred$mtry == models$rf$bestTune$mtry, "rowIndex"])]
    probabilities$train[["RF"]] <- rf_train_prob
    
    # 验证集预测
    rf_valid_prob <- predict(models$rf, newdata = valid_data, type = "prob")[, "Positive"]
    probabilities$valid[["RF"]] <- rf_valid_prob
    
  }, error = function(e) {
    cat("  随机森林训练错误:", e$message, "\n")
  })
  
  # 2) 支持向量机 (SVM)
  cat("\n2. 训练支持向量机模型...\n")
  tryCatch({
    models$svm <- train(
      formula,
      data = train_data,
      method = "svmRadial",
      trControl = train_control,
      metric = "ROC",
      tuneLength = 3
    )
    cat("  支持向量机训练完成\n")
    
    # 训练集预测（使用5折交叉验证的预测）
    svm_train_prob <- models$svm$pred[models$svm$pred$sigma == models$svm$bestTune$sigma &
                                        models$svm$pred$C == models$svm$bestTune$C, "Positive"]
    
    # 确保预测顺序与原始数据一致
    svm_train_prob <- svm_train_prob[order(models$svm$pred[models$svm$pred$sigma == models$svm$bestTune$sigma &
                                                             models$svm$pred$C == models$svm$bestTune$C, "rowIndex"])]
    probabilities$train[["SVM"]] <- svm_train_prob
    
    # 验证集预测
    svm_valid_prob <- predict(models$svm, newdata = valid_data, type = "prob")[, "Positive"]
    probabilities$valid[["SVM"]] <- svm_valid_prob
    
  }, error = function(e) {
    cat("  支持向量机训练错误:", e$message, "\n")
  })
  
  # 3) 多层感知机 (MLP)
  cat("\n3. 训练多层感知机模型...\n")
  tryCatch({
    models$mlp <- train(
      formula,
      data = train_data,
      method = "nnet",
      trControl = train_control,
      metric = "ROC",
      tuneLength = 3,
      trace = FALSE,
      linout = FALSE,
      maxit = 500
    )
    cat("  多层感知机训练完成\n")
    
    # 训练集预测（使用5折交叉验证的预测）
    mlp_train_prob <- models$mlp$pred[models$mlp$pred$size == models$mlp$bestTune$size &
                                        models$mlp$pred$decay == models$mlp$bestTune$decay, "Positive"]
    
    # 确保预测顺序与原始数据一致
    mlp_train_prob <- mlp_train_prob[order(models$mlp$pred[models$mlp$pred$size == models$mlp$bestTune$size &
                                                             models$mlp$pred$decay == models$mlp$bestTune$decay, "rowIndex"])]
    probabilities$train[["MLP"]] <- mlp_train_prob
    
    # 验证集预测
    mlp_valid_prob <- predict(models$mlp, newdata = valid_data, type = "prob")[, "Positive"]
    probabilities$valid[["MLP"]] <- mlp_valid_prob
    
  }, error = function(e) {
    cat("  多层感知机训练错误:", e$message, "\n")
  })
  
  # 4) 梯度提升机 (GBM)
  cat("\n4. 训练梯度提升模型...\n")
  tryCatch({
    models$gbm <- train(
      formula,
      data = train_data,
      method = "gbm",
      trControl = train_control,
      metric = "ROC",
      tuneLength = 3,
      verbose = FALSE
    )
    cat("  梯度提升模型训练完成\n")
    
    # 训练集预测（使用5折交叉验证的预测）
    gbm_train_prob <- models$gbm$pred[models$gbm$pred$n.trees == models$gbm$bestTune$n.trees &
                                        models$gbm$pred$interaction.depth == models$gbm$bestTune$interaction.depth &
                                        models$gbm$pred$shrinkage == models$gbm$bestTune$shrinkage &
                                        models$gbm$pred$n.minobsinnode == models$gbm$bestTune$n.minobsinnode, "Positive"]
    
    # 确保预测顺序与原始数据一致
    gbm_train_prob <- gbm_train_prob[order(models$gbm$pred[models$gbm$pred$n.trees == models$gbm$bestTune$n.trees &
                                                             models$gbm$pred$interaction.depth == models$gbm$bestTune$interaction.depth &
                                                             models$gbm$pred$shrinkage == models$gbm$bestTune$shrinkage &
                                                             models$gbm$pred$n.minobsinnode == models$gbm$bestTune$n.minobsinnode, "rowIndex"])]
    probabilities$train[["GBM"]] <- gbm_train_prob
    
    # 验证集预测
    gbm_valid_prob <- predict(models$gbm, newdata = valid_data, type = "prob")[, "Positive"]
    probabilities$valid[["GBM"]] <- gbm_valid_prob
    
  }, error = function(e) {
    cat("  梯度提升模型训练错误:", e$message, "\n")
  })
  
  # 5) XGBoost
  cat("\n5. 训练XGBoost模型...\n")
  tryCatch({
    # 准备数据矩阵
    train_x <- as.matrix(train_data[, feature_names])
    valid_x <- as.matrix(valid_data[, feature_names])
    
    # 创建xgboost矩阵
    dtrain <- xgb.DMatrix(data = train_x, label = train_y_numeric)
    dvalid <- xgb.DMatrix(data = valid_x, label = valid_y_numeric)
    
    # 设置XGBoost参数（使用新参数名evals）
    params <- list(
      objective = "binary:logistic",
      eval_metric = "auc",
      max_depth = 3,
      eta = 0.1,
      gamma = 0,
      colsample_bytree = 0.8,
      min_child_weight = 1,
      subsample = 0.8,
      nthread = 2
    )
    
    # 训练模型（使用新参数名evals）
    models$xgb <- xgb.train(
      params = params,
      data = dtrain,
      nrounds = 100,
      evals = list(train = dtrain, valid = dvalid), # 使用新参数名evals替代watchlist
      verbose = 0,
      print_every_n = 25
    )
    cat("  XGBoost训练完成\n")
    
    # 对于XGBoost，我们需要单独进行5折交叉验证来获取训练集的预测
    # 这里我们使用xgb.cv来获取交叉验证的预测
    xgb_cv <- xgb.cv(
      params = params,
      data = dtrain,
      nrounds = 100,
      nfold = 5,
      prediction = TRUE, # 保存预测结果
      showsd = TRUE,
      stratified = TRUE,
      verbose = 0
    )
    
    # 获取交叉验证的预测概率
    xgb_train_prob <- xgb_cv$pred
    probabilities$train[["XGB"]] <- xgb_train_prob
    
    # 验证集预测
    xgb_valid_prob <- predict(models$xgb, valid_x)
    probabilities$valid[["XGB"]] <- xgb_valid_prob
    
  }, error = function(e) {
    cat("  XGBoost训练错误:", e$message, "\n")
  })
  
  return(list(
    models = models,
    probabilities = probabilities,
    train_y = train_y_numeric,
    valid_y = valid_y_numeric
  ))
}

# 7. 训练所有模型
cat("\n=== 开始训练模型 ===\n")
results <- build_models(train_processed, valid_processed, feature_names, outcome_var)

# 8. 计算ROC曲线数据函数
calculate_roc_data <- function(probabilities, true_labels, dataset_name) {
  roc_data <- data.frame()
  
  for (model_name in names(probabilities)) {
    prob <- probabilities[[model_name]]
    true <- true_labels
    
    # 检查是否有有效的预测概率
    if (length(prob) > 0 && !all(is.na(prob)) && length(unique(true)) > 1) {
      tryCatch({
        # 确保预测概率和真实标签长度一致
        if (length(prob) != length(true)) {
          # 如果长度不一致，截断较长的那个
          min_length <- min(length(prob), length(true))
          prob <- prob[1:min_length]
          true <- true[1:min_length]
          cat("  注意: 模型", model_name, "在", dataset_name, "上的预测概率和真实标签长度不一致，已截断\n")
        }
        
        roc_obj <- roc(true ~ prob, quiet = TRUE, na.rm = TRUE)
        auc_val <- auc(roc_obj)
        
        # 提取ROC曲线坐标
        coords <- coords(roc_obj, "all", ret = c("specificity", "sensitivity"))
        temp_df <- data.frame(
          model = model_name,
          fpr = 1 - coords$specificity, # 转换为假阳性率
          tpr = coords$sensitivity,     # 真阳性率
          auc = round(auc_val, 3),
          dataset = dataset_name
        )
        
        roc_data <- rbind(roc_data, temp_df)
        cat("  ", model_name, "AUC:", round(auc_val, 3), "\n")
        
      }, error = function(e) {
        cat("  模型", model_name, "在", dataset_name, "上计算ROC时出错:", e$message, "\n")
      })
    } else {
      cat("  模型", model_name, "在", dataset_name, "上无法计算ROC\n")
    }
  }
  
  return(roc_data)
}

# 9. 绘制ROC曲线函数
plot_roc_curves <- function(roc_data, title) {
  if (nrow(roc_data) == 0) {
    cat("警告: 没有ROC数据可用于", title, "\n")
    return(NULL)
  }
  
  # 提取每个模型的AUC值
  auc_summary <- unique(roc_data[, c("model", "auc", "dataset")])
  
  p <- ggplot(roc_data, aes(x = fpr, y = tpr, color = model, group = model)) +
    geom_line(size = 1.2) + # 使用size保持向后兼容
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray50", size = 0.8) +
    labs(
      title = title,
      x = "False Positive Rate (1 - Specificity)",
      y = "True Positive Rate (Sensitivity)",
      color = "Model"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
      legend.position = "right",
      legend.title = element_text(face = "bold"),
      panel.grid.major = element_line(color = "gray90", linewidth = 0.2),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5),
      plot.margin = unit(c(20, 20, 20, 20), "pt")  # 修复：使用unit而不是margin
    ) +
    scale_color_brewer(palette = "Set1") +
    coord_equal(ratio = 1) +
    xlim(0, 1) +
    ylim(0, 1)
  
  # 在图中添加AUC值
  if (nrow(auc_summary) > 0) {
    auc_text <- paste(sapply(1:nrow(auc_summary), function(i) {
      sprintf("%s: %.3f", auc_summary$model[i], auc_summary$auc[i])
    }), collapse = "\n")
    
    p <- p + annotate("text",
                      x = 0.6, y = 0.2,
                      label = paste("AUC Values:\n", auc_text),
                      hjust = 0, vjust = 0, size = 4,
                      color = "black", fontface = "bold")
  }
  
  return(p)
}

# 10. 修复XGB训练集预测问题并计算ROC数据
cat("\n=== 修复XGB训练集预测问题 ===\n")

# 检查XGB训练集预测
if ("XGB" %in% names(results$probabilities$train)) {
  cat("XGB训练集预测数量:", length(results$probabilities$train[["XGB"]]), "\n")
  cat("训练集样本数量:", length(results$train_y), "\n")
  
  # 如果预测数量不匹配，重新计算XGB训练集预测
  if (length(results$probabilities$train[["XGB"]]) != length(results$train_y)) {
    cat("重新计算XGB训练集预测...\n")
    
    # 准备数据矩阵
    train_x <- as.matrix(train_processed[, feature_names])
    
    # 使用已训练的XGB模型进行预测
    results$probabilities$train[["XGB"]] <- predict(results$models$xgb, train_x)
    cat("重新计算后的XGB训练集预测数量:", length(results$probabilities$train[["XGB"]]), "\n")
  }
} else {
  cat("XGB训练集预测不存在，重新计算...\n")
  
  # 准备数据矩阵
  train_x <- as.matrix(train_processed[, feature_names])
  
  # 使用已训练的XGB模型进行预测
  if (!is.null(results$models$xgb)) {
    results$probabilities$train[["XGB"]] <- predict(results$models$xgb, train_x)
    cat("XGB训练集预测已计算，数量:", length(results$probabilities$train[["XGB"]]), "\n")
  }
}

# 计算训练集和验证集的ROC数据
cat("\n=== 计算训练集ROC ===\n")
train_roc_data <- calculate_roc_data(
  results$probabilities$train,
  results$train_y,
  "Training Set"
)

cat("\n=== 计算验证集ROC ===\n")
valid_roc_data <- calculate_roc_data(
  results$probabilities$valid,
  results$valid_y,
  "Validation Set"
)

# 11. 创建并保存图形
cat("\n=== 绘制ROC曲线 ===\n")

# 训练集ROC曲线
train_plot <- plot_roc_curves(train_roc_data, "ROC Curves - Training Set (5-Fold CV)")

# 验证集ROC曲线
valid_plot <- plot_roc_curves(valid_roc_data, "ROC Curves - Validation Set")

# 保存为PNG文件
if (!is.null(train_plot)) {
  png("training_roc_curves_5fold_cv.png", width = 1000, height = 800, res = 150)
  print(train_plot)
  dev.off()
  cat("\n训练集ROC曲线已保存: training_roc_curves_5fold_cv.png\n")
} else {
  cat("\n无法保存训练集ROC曲线\n")
}

if (!is.null(valid_plot)) {
  png("validation_roc_curves.png", width = 1000, height = 800, res = 150)
  print(valid_plot)
  dev.off()
  cat("验证集ROC曲线已保存: validation_roc_curves.png\n")
} else {
  cat("无法保存验证集ROC曲线\n")
}

# 12. 打印模型性能摘要
cat("\n=== 模型性能摘要 ===\n")

cat("\n训练集AUC (5折交叉验证):\n")
if (nrow(train_roc_data) > 0) {
  auc_train <- unique(train_roc_data[, c("model", "auc")])
  for (i in 1:nrow(auc_train)) {
    cat(sprintf("%s: %.3f\n", auc_train$model[i], auc_train$auc[i]))
  }
} else {
  cat("无可用数据\n")
}

cat("\n验证集AUC:\n")
if (nrow(valid_roc_data) > 0) {
  auc_valid <- unique(valid_roc_data[, c("model", "auc")])
  for (i in 1:nrow(auc_valid)) {
    cat(sprintf("%s: %.3f\n", auc_valid$model[i], auc_valid$auc[i]))
  }
} else {
  cat("无可用数据\n")
}

# 13. 显示图形（可选）
if (!is.null(train_plot) && !is.null(valid_plot)) {
  # 使用patchwork将两个图形并排显示
  combined_plot <- train_plot + valid_plot +
    plot_layout(guides = 'collect') &
    theme(legend.position = 'bottom')
  
  png("combined_roc_curves.png", width = 1600, height = 800, res = 150)
  print(combined_plot)
  dev.off()
  cat("\n组合ROC曲线已保存: combined_roc_curves.png\n")
  
  # 在R中显示图形
  print(train_plot)
  print(valid_plot)
  print(combined_plot)
}

# 14. 打印训练成功的模型
cat("\n=== 成功训练的模型 ===\n")
model_names <- c("RF", "SVM", "MLP", "GBM", "XGB")
for (model in model_names) {
  model_key <- tolower(model)
  if (model == "XGB") {
    if (!is.null(results$models$xgb)) {
      cat(sprintf("%s: 训练成功\n", model))
    } else {
      cat(sprintf("%s: 训练失败\n", model))
    }
  } else {
    if (!is.null(results$models[[model_key]])) {
      cat(sprintf("%s: 训练成功\n", model))
    } else {
      cat(sprintf("%s: 训练失败\n", model))
    }
  }
}

# 15. 保存预处理和模型结果
saveRDS(prep_trained, file = "preprocessing_recipe.rds")
saveRDS(results, file = "model_results.rds")
cat("\n预处理方案和模型结果已保存到当前目录\n")

# 16. 显示数据预处理信息
cat("\n=== 数据预处理详情 ===\n")
cat("连续变量标准化:", paste(continuous_vars, collapse = ", "), "\n")
cat("分类变量独热编码:", paste(categorical_vars, collapse = ", "), "\n")
cat("处理后特征数量:", length(feature_names), "\n")
cat("原始数据维度 - 训练集:", dim(train_data), "验证集:", dim(valid_data), "\n")
cat("处理后数据维度 - 训练集:", dim(train_processed), "验证集:", dim(valid_processed), "\n")

# 17. 打印模型训练详情
cat("\n=== 模型训练详情 ===\n")
cat("训练集样本数:", length(results$train_y), "\n")
cat("验证集样本数:", length(results$valid_y), "\n")
cat("正类比例 - 训练集:", mean(results$train_y == 1), "\n")
cat("正类比例 - 验证集:", mean(results$valid_y == 1), "\n")

# 18. 检查各模型预测概率的长度
cat("\n=== 各模型预测概率长度检查 ===\n")
cat("训练集:\n")
for (model_name in names(results$probabilities$train)) {
  if (!is.null(results$probabilities$train[[model_name]])) {
    cat(sprintf("  %s: %d\n", model_name, length(results$probabilities$train[[model_name]])))
  }
}

cat("\n验证集:\n")
for (model_name in names(results$probabilities$valid)) {
  if (!is.null(results$probabilities$valid[[model_name]])) {
    cat(sprintf("  %s: %d\n", model_name, length(results$probabilities$valid[[model_name]])))
  }
}