# 加载必要的包
library(readxl)
library(rms)

# 1. 读取模型和数据
model_path <- "C:/data/vt_sgd_prediction_model.rds"
train_path <- "C:/data/lyq0118.xlsx"
test_path <- "C:/data/sty0118.xlsx"

# 读取模型
model <- readRDS(model_path)

# 读取训练集和验证集数据
train_data <- read_excel(train_path)
test_data <- read_excel(test_path)

# 2. 确保变量名正确并转换为适当的格式
# 调整变量名以匹配实际数据（根据实际情况修改）
colnames(train_data) <- tolower(colnames(train_data))
colnames(test_data) <- tolower(colnames(test_data))

# 假设变量名为：hrev（结局），vt（二分类），sgd（连续）
# 确保vt是因子变量，hrev是因子变量
train_data$vt <- as.factor(train_data$vt)
train_data$hrev <- as.factor(train_data$hrev)
test_data$vt <- as.factor(test_data$vt)
test_data$hrev <- as.factor(test_data$hrev)

# 3. 使用模型进行预测
# 对训练集进行预测
train_pred <- predict(model, newdata = train_data, type = "response")
# 对验证集进行预测
test_pred <- predict(model, newdata = test_data, type = "response")

# 4. 准备数据
# 创建训练集的概率和实际值
train_prob <- as.numeric(train_pred)
train_actual <- as.numeric(as.character(train_data$hrev))

# 创建验证集的概率和实际值
test_prob <- as.numeric(test_pred)
test_actual <- as.numeric(as.character(test_data$hrev))

# 5. 绘制校准曲线
# 创建图形窗口
par(mfrow = c(1, 2))

# 训练集校准曲线（使用正确的参数格式）
val.prob(p = train_prob, y = train_actual, 
         smooth = TRUE,  # 应该是逻辑值，不是字符串
         logistic.cal = TRUE,
         legendloc = FALSE,
         statloc = FALSE,
         xlab = "Predicted Probability",
         ylab = "Actual Probability")
title("Training Set Calibration Curve")

# 验证集校准曲线
val.prob(p = test_prob, y = test_actual, 
         smooth = TRUE,  # 应该是逻辑值，不是字符串
         logistic.cal = TRUE,
         legendloc = FALSE,
         statloc = FALSE,
         xlab = "Predicted Probability",
         ylab = "Actual Probability")
title("Validation Set Calibration Curve")

# 重置图形参数
par(mfrow = c(1, 1))

# 6. 计算Brier评分
# 计算训练集Brier评分
train_brier <- mean((train_prob - train_actual)^2)
cat("Training Set Brier Score:", round(train_brier, 4), "\n")

# 计算验证集Brier评分
test_brier <- mean((test_prob - test_actual)^2)
cat("Validation Set Brier Score:", round(test_brier, 4), "\n")

# 7. 输出性能指标总结
cat("\n=== Model Performance Summary ===\n")
cat("Training Set:\n")
cat("  Brier Score:", round(train_brier, 4), "\n")
cat("  Number of observations:", length(train_prob), "\n")

cat("\nValidation Set:\n")
cat("  Brier Score:", round(test_brier, 4), "\n")
cat("  Number of observations:", length(test_prob), "\n")