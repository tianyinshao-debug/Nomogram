library(readxl)
library(openxlsx)
library(plyr)
library(glmnet)
library(ggplot2) 
library(dplyr)   
library(survival)
library(survminer)
library(corrplot)
library(ggplot2)
library(Hmisc)
library(skimr)
library(corrplot)
library(glmnet)
library(caret)
library(CBCgrps)
library(nortest)
library(tidyverse)
library(ggpubr)
library(rms)
library(pROC)
library(viridis)
library(glm2) 
library(broom)
library(tableone)
library(survey)
library(reshape2)
library(MatchIt)
#读取数据
data <- read_excel("~/Desktop/科研/列线图/514.xlsx")
#把分类型变量转化成因子型
data$Hospital<-as.factor(data$Hospital)

# 从固定路径读取训练集和验证集
train_data <- read_excel("C:/data/train_data.xlsx")
test_data <- read_excel("C:/data/test_data.xlsx")


#####列线图
library(pec)
library(rms)
library(survival)
train_data$OS_time <- train_data$OS  
train_data$OS_event <- train_data$SUV
test_data$OS_time <- test_data$OS  
test_data$OS_event <- test_data$SUV

selected_vars <- c("Sgm","Gender","AFP","CP","TN","MVI")
ddist <- datadist(train_data[,c("OS","SUV",selected_vars)])
options(datadist='ddist')
cox_model <- cph(Surv(OS, SUV) ~  Sgm + CP + AFP + TN + Gender + MVI,
                 data = train_data, 
                 x = TRUE, y = TRUE, 
                 surv = TRUE) 

# 在Cox模型中，中位生存时间是通过求解 S(t) = 0.5 得到的
median_surv <- function(linear.predictor) {
  # 获取基准生存曲线
  base_surv <- survfit(cox_model)
  base_times <- base_surv$time
  base_survival <- base_surv$surv
  
  # 对于每个线性预测值，计算中位生存时间
  median_times <- sapply(linear.predictor, function(lp) {
    # 计算该患者的生存曲线
    patient_surv <- base_survival^exp(lp)
    
    # 找到生存概率首次低于0.5的时间点
    idx <- which(patient_surv <= 0.5)[1]
    ifelse(is.na(idx), max(base_times), base_times[idx])
  })
  
  return(median_times)
}
# 定义3年和5年生存率函数
surv3_year <- function(lp) {
  s <- survest(cox_model, times = 36, linear.predictors = lp)
  return(as.numeric(s$surv))
}

surv5_year <- function(lp) {
  s <- survest(cox_model, times = 60, linear.predictors = lp)
  return(as.numeric(s$surv))
}

# 绘制列线图
nomogram_obj <- nomogram(cox_model,
                         fun = list(median_surv,
                                    surv3_year, 
                                    surv5_year),
                         funlabel = c("Expected Survival Time (months)",
                                      "3-Year OS", 
                                      "5-Year OS"),
                         lp = TRUE,
                         maxscale = 100,
                         fun.at = list( seq(0, 150, by = 20),
                                        seq(0, 1, by=0.1),
                                        seq(0, 1, by=0.1)) 
)
plot(nomogram_obj, 
     lplabel = "Linear Predictor",
     xfrac = 0.2, 
     cex.axis = 0.8, 
     cex.var = 0.8)
# 线性预测器（lp）为0
linear_predictor_0 <- 0
# 计算3年生存率
surv_3year_0 <- surv3_year(linear_predictor_0)
# 计算5年生存率  
surv_5year_0 <- surv5_year(linear_predictor_0)
# 计算期望生存时间（中位生存时间）
expected_survival_time <- median_surv(linear_predictor_0)
# 定义变量
vars <- c(Sgm = -0.641, CP = 0.891, AFP = 0.603, TN = 0.681, Gender = 0.625, MVI = 0.865)
n <- length(vars)
# 生成所有组合（每个变量有两种状态：取原值或取0）
combinations <- expand.grid(replicate(n, list(c(FALSE, TRUE))))
names(combinations) <- names(vars)
# 计算每种组合的和
sums <- apply(combinations, 1, function(row) {
  sum(vars * row)  # TRUE=1, FALSE=0，所以只加被选中的变量
})

# 创建结果数据框
results_3year <- data.frame(
  Combination = apply(combinations, 1, function(row) {
    selected <- names(vars)[row]
    if (length(selected) == 0) "无变量" else paste(selected, collapse = " + ")
  }),
  Sum = sums,
  stringsAsFactors = FALSE
)
lp <- results_3year[,-1]

# 定义生存率函数（请根据您的实际模型调整参数）
surv3_year <- function(lp) {
  base_survival <- 0.777  # 3年基准生存率
  hazard_ratio <- exp(lp)
  return(base_survival^hazard_ratio)
}
surv5_year <- function(lp) {
  base_survival <- 0.649  # 5年基准生存率
  hazard_ratio <- exp(lp)
  return(base_survival^hazard_ratio)
}

Survival <- sapply(lp, surv3_year)
points_data <- data.frame(lp,Survival)
Survival_5 <- sapply(lp, surv5_year)
points_data_5 <- data.frame(lp,Survival_5)
Survival_time <- sapply(lp,median_surv)
points_data_time <- data.frame(lp,Survival_time)
# 进行三次多项式拟合
fit <- lm(Survival ~ poly(lp, 3, raw = TRUE), data = points_data)
fit_5 <- lm(Survival_5 ~ poly(lp, 3, raw = TRUE), data = points_data_5)
fit_time <- lm(Survival_time ~ poly(lp, 3, raw = TRUE), data = points_data_time)
# 提取系数
coefficients <- coef(fit)
cat("拟合公式系数:\n")
cat(sprintf("a (Points³系数) = %.6f\n", coefficients[4]))
cat(sprintf("b (Points²系数) = %.6f\n", coefficients[3]))
cat(sprintf("c (Points系数) = %.6f\n", coefficients[2]))
cat(sprintf("d (截距) = %.6f\n", coefficients[1]))

coefficients_5 <- coef(fit_5)
cat("拟合公式系数:\n")
cat(sprintf("a (Points³系数) = %.6f\n", coefficients_5[4]))
cat(sprintf("b (Points²系数) = %.6f\n", coefficients_5[3]))
cat(sprintf("c (Points系数) = %.6f\n", coefficients_5[2]))
cat(sprintf("d (截距) = %.6f\n", coefficients_5[1]))

coefficients_time <- coef(fit_time)
cat("拟合公式系数:\n")
cat(sprintf("a (Points³系数) = %.6f\n", coefficients_time[4]))
cat(sprintf("b (Points²系数) = %.6f\n", coefficients_time[3]))
cat(sprintf("c (Points系数) = %.6f\n", coefficients_time[2]))
cat(sprintf("d (截距) = %.6f\n", coefficients_time[1]))

# 计算R²
r_squared <- summary(fit)$r.squared
cat(sprintf("\nR² = %.6f\n", r_squared))
r_squared_5 <- summary(fit_5)$r.squared
cat(sprintf("\nR² = %.6f\n", r_squared_5))
r_squared_time <- summary(fit_time)$r.squared
cat(sprintf("\nR² = %.6f\n", r_squared_time))
# 创建更平滑的拟合曲线数据
x_fit <- seq(min(points_data$lp), max(points_data$lp), length.out = 500)
y_fit <- predict(fit, newdata = data.frame(lp = x_fit))
fit_data <- data.frame(Points = x_fit, Survival = y_fit)

x_fit_5 <- seq(min(points_data_5$lp), max(points_data_5$lp), length.out = 500)
y_fit_5 <- predict(fit_5, newdata = data.frame(lp = x_fit_5))
fit_data_5 <- data.frame(Points = x_fit_5, Survival = y_fit_5)

x_fit_time <- seq(min(points_data_time$lp), max(points_data_time$lp), length.out = 500)
y_fit_time <- predict(fit_time, newdata = data.frame(lp = x_fit_time))
fit_data_time <- data.frame(Points = x_fit_time, Survival = y_fit_time)

# 绘制散点图和拟合曲线
ggplot() +
  geom_point(data = points_data, aes(x = lp, y = Survival), 
             color = "blue", alpha = 0.7, size = 2) +
  geom_line(data = fit_data, aes(x = Points, y = Survival), 
            color = "red", linewidth = 1.2) +
  labs(x = "Linear predictor (LP)", 
       y = "3-year survival probability",
       caption = paste0("3-year survival probability = ", 
                        sprintf("%.4f", coefficients[4]), "× LP³ + ",
                        sprintf("%.4f", coefficients[3]), "× LP² + ",
                        sprintf("%.4f", coefficients[2]), "× LP + ",
                        sprintf("%.4f", coefficients[1]))) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 14),
        plot.caption = element_text(hjust = 0, size = 10, face = "italic"))+
  ylim(-0.1,1)

ggplot() +
  geom_point(data = points_data_5, aes(x = lp, y = Survival_5), 
             color = "blue", alpha = 0.7, size = 2) +
  geom_line(data = fit_data_5, aes(x = Points, y = Survival), 
            color = "red", linewidth = 1.2) +
  labs(x = "Linear predictor (LP)", 
       y = "5-year survival probability",
       caption = paste0("5-year survival probability = ", 
                        sprintf("%.4f", coefficients_5[4]), "× LP³ + ",
                        sprintf("%.4f", coefficients_5[3]), "× LP² + ",
                        sprintf("%.4f", coefficients_5[2]), "× LP + ",
                        sprintf("%.4f", coefficients_5[1]))) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 14),
        plot.caption = element_text(hjust = 0, size = 10, face = "italic")) +
  ylim(-0.1,1)
# 首先为两个数据集添加一个标识变量
points_data$Year <- "3-year"
points_data_5$Year <- "5-year"

# 合并散点数据
combined_points <- rbind(
  data.frame(lp = points_data$lp, Survival = points_data$Survival, Year = "3-year"),
  data.frame(lp = points_data_5$lp, Survival = points_data_5$Survival_5, Year = "5-year")
)

# 合并拟合曲线数据
combined_fit <- rbind(
  data.frame(Points = fit_data$Points, Survival = fit_data$Survival, Year = "3-year"),
  data.frame(Points = fit_data_5$Points, Survival = fit_data_5$Survival, Year = "5-year")
)

# 绘制组合图
ggplot() +
  geom_point(data = combined_points, 
             aes(x = lp, y = Survival, color = Year), 
             alpha = 0.7, size = 2) +
  geom_line(data = combined_fit, 
            aes(x = Points, y = Survival, color = Year), 
            linewidth = 1.2) + 
  annotate("text", x = 2, y = 0.95, 
           label = paste("3-year R² =", sprintf("%.4f", r_squared)),
           hjust = 0, vjust = 1, size = 4, color = "red", fontface = "bold") +
  annotate("text", x = 2, y = 0.85, 
           label = paste("5-year R² =", sprintf("%.4f", r_squared_5)),
           hjust = 0, vjust = 1, size = 4, color = "blue", fontface = "bold") +
  scale_color_manual(values = c("3-year" = "red", "5-year" = "blue")) +
  labs(x = "Linear predictor (LP)", 
       y = "Survival probability",
       color = "Time Period",
       caption = paste0("3-year: ", 
                        sprintf("%.4f", coefficients[4]), "× LP³ + ",
                        sprintf("%.4f", coefficients[3]), "× LP² + ",
                        sprintf("%.4f", coefficients[2]), "× LP + ",
                        sprintf("%.4f", coefficients[1]),
                        "\n5-year: ",
                        sprintf("%.4f", coefficients_5[4]), "× LP³ + ",
                        sprintf("%.4f", coefficients_5[3]), "× LP² + ",
                        sprintf("%.4f", coefficients_5[2]), "× LP + ",
                        sprintf("%.4f", coefficients_5[1]))) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 14),
        plot.caption = element_text(hjust = 0, size = 10, face = "italic"),
        legend.position = "bottom") +
  ylim(-0.1, 1)

ggplot() +
  geom_point(data = points_data_time, aes(x = lp, y = Survival_time), 
             color = "darkred", alpha = 0.7, size = 2) +
  geom_line(data = fit_data_time, aes(x = Points, y = Survival), 
            color = "darkred", linewidth = 1.2) +
  annotate("text", x = 2, y = 150, 
           label = paste("R² =", sprintf("%.4f", r_squared_time)),
           hjust = 0, vjust = 1, size = 4, color = "darkred", fontface = "bold")+
  labs(x = "Linear predictor (LP)", 
       y = "Expected survival time",
       caption = paste0("Expected survival time = ", 
                        sprintf("%.4f", coefficients_time[4]), "× LP³ + ",
                        sprintf("%.4f", coefficients_time[3]), "× LP² + ",
                        sprintf("%.4f", coefficients_time[2]), "× LP + ",
                        sprintf("%.4f", coefficients_time[1]))) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 14),
        plot.caption = element_text(hjust = 0, size = 10, face = "italic"))


###################################绘制决策曲线
library(rmda) 
library(pec)
# 确保Child-Pugh评分是分类变量
train_data$CPAB <- factor(train_data$CP, levels=c(0,1))
test_data$CPAB <- factor(test_data$CP, levels=c(0,1))
train_data$BCLC_cat <- factor(train_data$BCLC, levels = c(0,1))
test_data$BCLC_cat <- factor(test_data$BCLC, levels = c(0,1))
# 计算各组3年OS的生存概率（Kaplan-Meier估计）
library(survival)
km_fit_train_cp <- survfit(Surv(OS12, SUV) ~ CPAB, data = train_data)
km_fit_test_cp <- survfit(Surv(OS12, SUV) ~ CPAB, data = test_data)
km_fit_train_bclc <- survfit(Surv(OS12, SUV) ~ BCLC_cat, data = train_data)
km_fit_test_bclc <- survfit(Surv(OS12, SUV) ~ BCLC_cat, data = test_data)
####1os
# 1. 获取各Child-Pugh评分组的3,5年生存概率
km_summary_train_3_cp <- summary(km_fit_train_cp, times = 3)  
km_summary_test_3_cp <- summary(km_fit_test_cp, times = 3)
km_summary_train_5_cp <- summary(km_fit_train_cp, times = 5)  
km_summary_test_5_cp <- summary(km_fit_test_cp, times = 5)

km_summary_train_3_bclc <- summary(km_fit_train_bclc, times = 3) 
km_summary_test_3_bclc <- summary(km_fit_test_bclc, times = 3) 
km_summary_train_5_bclc <- summary(km_fit_train_bclc, times = 5) 
km_summary_test_5_bclc <- summary(km_fit_test_bclc, times = 5) 

# 2. 提取分组名称和对应的生存概率
cpab_groups_train_3 <- names(km_fit_train_cp$strata)  # 获取分组名称（如 "CPscore_cat=5"）
surv_prob_train_3_cp <- km_summary_train_3_cp$surv        # 对应的生存概率
cpab_groups_test_3 <- names(km_fit_test_cp$strata)  # 获取分组名称（如 "CPscore_cat=5"）
surv_prob_test_3_cp <- km_summary_test_3_cp$surv
cpab_groups_train_5 <- names(km_fit_train_cp$strata)  # 获取分组名称（如 "CPscore_cat=5"）
surv_prob_train_5_cp <- km_summary_train_5_cp$surv        # 对应的生存概率
cpab_groups_test_5 <- names(km_fit_test_cp$strata)  # 获取分组名称（如 "CPscore_cat=5"）
surv_prob_test_5_cp <- km_summary_test_5_cp$surv 

bclc_groups_train_3 <- names(km_fit_train_bclc$strata)  # 获取分组名称（如 "CPscore_cat=5"）
surv_prob_train_3_bclc <- km_summary_train_3_bclc$surv        # 对应的生存概率
bclc_groups_train_5 <- names(km_fit_train_bclc$strata)  # 获取分组名称（如 "CPscore_cat=5"）
surv_prob_train_5_bclc <- km_summary_train_5_bclc$surv        # 对应的生存概率
bclc_groups_test_3 <- names(km_fit_test_bclc$strata)  # 获取分组名称（如 "CPscore_cat=5"）
surv_prob_test_3_bclc <- km_summary_test_3_bclc$surv        # 对应的生存概率
bclc_groups_test_5 <- names(km_fit_test_bclc$strata)  # 获取分组名称（如 "CPscore_cat=5"）
surv_prob_test_5_bclc <- km_summary_test_5_bclc$surv        # 对应的生存概率


# 3. 清理分组名称（去掉前缀）
cpab_levels_train_3 <- gsub("CPAB=", "", cpab_groups_train_3)
cpab_levels_test_3 <- gsub("CPAB=", "", cpab_groups_test_3)
cpab_levels_train_5 <- gsub("CPAB=", "", cpab_groups_train_5)
cpab_levels_test_5 <- gsub("CPAB=", "", cpab_groups_test_5)

bclc_levels_train_3 <- gsub("BCLC_cat=", "", bclc_groups_train_3)
bclc_levels_train_5 <- gsub("BCLC_cat=", "", bclc_groups_train_5)
bclc_levels_test_3 <- gsub("BCLC_cat=", "", bclc_groups_test_3)
bclc_levels_test_5 <- gsub("BCLC_cat=", "", bclc_groups_test_5)

# 4. 创建映射数据框
cpab_surv_map_train_3 <- data.frame(
  CPAB_cat = factor(cpab_levels_train_3, levels = c(0,1)),
  surv_3year_train_cp = surv_prob_train_3_cp
)
cpab_surv_map_test_3 <- data.frame(
  CPAB_cat = factor(cpab_levels_test_3, levels = c(0,1)),
  surv_3year_test_cp = surv_prob_test_3_cp
)
cpab_surv_map_train_5 <- data.frame(
  CPAB_cat = factor(cpab_levels_train_5, levels = c(0,1)),
  surv_5year_train_cp = surv_prob_train_5_cp
)
cpab_surv_map_test_5 <- data.frame(
  CPAB_cat = factor(cpab_levels_test_5, levels = c(0,1)),
  surv_5year_test_cp = surv_prob_test_5_cp
)

bclc_surv_map_train_3 <- data.frame(
  BCLC_cat = factor(bclc_levels_train_3, levels = c(0,1)),
  surv_3year_train_bclc = surv_prob_train_3_bclc
)
bclc_surv_map_train_5 <- data.frame(
  BCLC_cat = factor(bclc_levels_train_5, levels = c(0,1)),
  surv_5year_train_bclc = surv_prob_train_5_bclc
)
bclc_surv_map_test_3 <- data.frame(
  BCLC_cat = factor(bclc_levels_test_3, levels = c(0,1)),
  surv_3year_test_bclc = surv_prob_test_3_bclc
)
bclc_surv_map_test_5 <- data.frame(
  BCLC_cat = factor(bclc_levels_test_5, levels = c(0,1)),
  surv_5year_test_bclc = surv_prob_test_5_bclc
)


# 5. 将生存概率匹配回原始数据
train_data$surv_3year_cpab <- cpab_surv_map_train_3$surv_3year_train_cp[match(train_data$CPAB, cpab_surv_map_train_3$CPAB_cat)]
test_data$surv_3year_cpab <- cpab_surv_map_test_3$surv_3year_test_cp[match(test_data$CPAB, cpab_surv_map_test_3$CPAB_cat)]
train_data$surv_5year_cpab <- cpab_surv_map_train_5$surv_5year_train_cp[match(train_data$CPAB, cpab_surv_map_train_5$CPAB_cat)]
test_data$surv_5year_cpab <- cpab_surv_map_test_5$surv_5year_test_cp[match(test_data$CPAB, cpab_surv_map_test_5$CPAB_cat)]

train_data$surv_3year_bclc <- bclc_surv_map_train_3$surv_3year_train[match(train_data$BCLC_cat, bclc_surv_map_train_3$BCLC_cat)]
train_data$surv_5year_bclc <- bclc_surv_map_train_5$surv_5year_train[match(train_data$BCLC_cat, bclc_surv_map_train_5$BCLC_cat)]
test_data$surv_3year_bclc <- bclc_surv_map_test_3$surv_3year_test[match(test_data$BCLC_cat, bclc_surv_map_test_3$BCLC_cat)]
test_data$surv_5year_bclc <- bclc_surv_map_test_5$surv_5year_test[match(test_data$BCLC_cat, bclc_surv_map_test_5$BCLC_cat)]

# 6. 计算预测风险概率（1 - 生存概率）
train_data$pred_cpab_km_train_3 <- 1 - train_data$surv_3year_cpab
test_data$pred_cpab_km_test_3 <- 1 - test_data$surv_3year_cpab
train_data$pred_cpab_km_train_5 <- 1 - train_data$surv_5year_cpab
test_data$pred_cpab_km_test_5 <- 1 - test_data$surv_5year_cpab
train_data$pred_bclc_km_train_3 <- 1 - train_data$surv_3year_bclc
train_data$pred_bclc_km_train_5 <- 1 - train_data$surv_5year_bclc
test_data$pred_bclc_km_test_3 <- 1 - test_data$surv_3year_bclc
test_data$pred_bclc_km_test_5 <- 1 - test_data$surv_5year_bclc

#####
dca_cpab_3_train <- decision_curve(SUV ~ pred_cpab_km_train_3, data = train_data, thresholds = seq(0, 0.5, 0.01))
dca_cpab_3_test <- decision_curve(SUV ~ pred_cpab_km_test_3, data = test_data, thresholds = seq(0, 0.5, 0.01))
dca_cpab_5_train <- decision_curve(SUV ~ pred_cpab_km_train_5, data = train_data, thresholds = seq(0, 0.5, 0.01))
dca_cpab_5_test <- decision_curve(SUV ~ pred_cpab_km_test_5, data = test_data, thresholds = seq(0, 0.5, 0.01))
dca_bclc_3_train <- decision_curve(SUV ~ pred_bclc_km_train_3, data = train_data, thresholds = seq(0, 0.5, 0.01))
dca_bclc_5_train <- decision_curve(SUV ~ pred_bclc_km_train_5, data = train_data, thresholds = seq(0, 0.5, 0.01))
dca_bclc_3_test <- decision_curve(SUV ~ pred_bclc_km_test_3, data = test_data, thresholds = seq(0, 0.5, 0.01))
dca_bclc_5_test <- decision_curve(SUV ~ pred_bclc_km_test_5, data = test_data, thresholds = seq(0, 0.5, 0.01))


###############
cox_model_3 <- cph(Surv(OS12, SUV) ~  Sgm + Gender+ CP + AFP + TN  + MVI,
                   data = train_data, 
                   x = TRUE, y = TRUE, 
                   surv = TRUE) 
cox_model_5 <- cph(Surv(OS12, SUV) ~  Sgm + Gender+ CP + AFP + TN  + MVI,
                   data = train_data, 
                   x = TRUE, y = TRUE, 
                   surv = TRUE) 
pred_3year <- predictSurvProb(cox_model_3, newdata = train_data, times = 3)
pred_5year <- predictSurvProb(cox_model_5, newdata = train_data, times = 5)
pred_3year_test <- predictSurvProb(cox_model_3, newdata = test_data, times = 3)
pred_5year_test <- predictSurvProb(cox_model_5, newdata = test_data, times = 5)

# Create data frames for DCA
# 3 year
dca_data_3year <- data.frame(event = train_data$OS_event,time = train_data$OS12,model = pred_3year,all = mean(pred_3year),none = 0)
dca_3year <- decision_curve(formula = event ~ model +all + none, data = dca_data_3year,thresholds = seq(0, 1, by = 0.01),policy = "opt-in", bootstraps = 500)
dca_data_3year_test <- data.frame(event = test_data$SUV,time = test_data$OS12,model = pred_3year_test,all = mean(pred_3year_test),none = 0)
dca_3year_test <- decision_curve(formula = event ~ model + all + none,  data = dca_data_3year_test,thresholds = seq(0, 1, by = 0.01),policy = "opt-in", bootstraps = 500)

# 5 year
dca_data_5year <- data.frame(event = train_data$OS_event,time = train_data$OS12,model = pred_5year,all = mean(pred_5year),none = 0)
dca_5year <- decision_curve(formula = event ~ model +all + none, data = dca_data_5year,thresholds = seq(0, 1, by = 0.01),policy = "opt-in", bootstraps = 500)
dca_data_5year_test <- data.frame(event = test_data$OS_event,time = test_data$OS12,model = pred_5year_test,all = mean(pred_5year_test),none = 0)
dca_5year_test <- decision_curve(formula = event ~ model + all + none,  data = dca_data_5year_test,thresholds = seq(0, 1, by = 0.01),policy = "opt-in", bootstraps = 500)

plot_decision_curve(list(dca_3year, dca_cpab_3_train, dca_bclc_3_train), 
                    curve.names = c("Nomogram", "CPC", "BCLC","Treat All", "Treat None"),
                    cost.benefit.axis = FALSE,
                    col = c("darkred", "darkorange","skyblue", "darkblue", "darkgreen"),
                    confidence.intervals = FALSE,
                    standardize = FALSE,
                    xlim = c(0, 1),
                    ylim = c(-0.12, 0.5),
                    main = "3-Year OS (training cohort)")
plot_decision_curve(list(dca_3year_test, dca_cpab_3_test, dca_bclc_3_test), 
                    curve.names = c("Nomogram", "CPC", "BCLC","Treat All", "Treat None"),
                    cost.benefit.axis = FALSE,
                    col = c("darkred", "darkorange","skyblue", "darkblue", "darkgreen"),
                    confidence.intervals = FALSE,
                    standardize = FALSE,
                    xlim = c(0, 1),
                    ylim = c(-0.12, 0.5),
                    main = "3-Year OS (validation cohort)")
plot_decision_curve(list(dca_5year, dca_cpab_5_train, dca_bclc_5_train), 
                    curve.names = c("Nomogram", "CPC", "BCLC","Treat All", "Treat None"),
                    cost.benefit.axis = FALSE,
                    col = c("darkred", "darkorange","skyblue", "darkblue", "darkgreen"),
                    confidence.intervals = FALSE,
                    standardize = FALSE,
                    xlim = c(0, 1),
                    ylim = c(-0.12, 0.5),
                    main = "5-Year OS (training cohort)")
plot_decision_curve(list(dca_5year_test, dca_cpab_5_test, dca_bclc_5_test), 
                    curve.names = c("Nomogram", "CPC","BCLC" ,"All", "None"),
                    cost.benefit.axis = FALSE,
                    col = c("darkred", "darkorange","skyblue", "darkblue", "darkgreen"),
                    confidence.intervals = FALSE,
                    standardize = FALSE,
                    xlim = c(0, 1),
                    ylim = c(-0.12, 0.5),
                    main = "5-Year OS (validation cohort)")

######################################绘制校准曲线
cal3 <- rms::calibrate(cox_model_3,data = train_data,
                       formula = Surv(OS12, OS_event) ~ Sgm + CP + TN + AFP + Gender + MVI,
                       u = 3,method = "boot",B = 500)
cal3_test <- rms::calibrate(cox_model_3, data = test_data,
                            formula = Surv(OS12, OS_event) ~ Sgm + CP + TN + AFP + Gender + MVI,
                            u = 3,  method = "boot",B = 500)
cal5 <- rms::calibrate(cox_model_5, data = train_data,
                       formula = Surv(OS12, OS_event) ~ Sgm + CP + TN + AFP + Gender + MVI,
                       u = 5,method = "boot",B = 500)
cal5_test <- rms::calibrate(cox_model_5, data = test_data,
                            formula = Surv(OS12, OS_event) ~ Sgm + CP + TN + AFP + Gender + MVI,
                            u = 5,  method = "boot",B = 500)

# 3年校准曲线
plot(cal3, 
     xlim=c(0,1), ylim=c(0,1),
     xlab="Nomogram-predicted OS Probability",
     ylab="Actual OS Probability",
     main="3-Year OS (training cohort)")
plot(cal3_test, 
     xlim=c(0,1), ylim=c(0,1),
     xlab="Nomogram-predicted OS Probability",
     ylab="Actual OS Probability",
     main="3-Year OS (validation cohort)")
# 5年校准曲线
plot(cal5, 
     xlim=c(0,1), ylim=c(0,1),
     xlab="Nomogram-predicted OS Probability",
     ylab="Actual OS Probability",
     main="5-Year OS (training cohort)")
plot(cal5_test, 
     xlim=c(0,1), ylim=c(0,1),
     xlab="Nomogram-predicted OS Probability",
     ylab="Actual OS Probability",
     main="5-Year OS (validation cohort)")

###############################ROC曲线
library(survival)
library(rms)
library(timeROC)
library(ggplot2)
library(dplyr)
train_data$predicted_risk <- predict(cox_model, newdata = train_data)
test_data$predicted_risk <- predict(cox_model, newdata = test_data)
# 定义时间
times <- c(3,5)
# 计算时间依赖性 ROC
library(timeROC)
# 计算不同时间点的ROC曲线
roc_3year <- timeROC(T = train_data$OS12,
                     delta = train_data$OS_event,
                     marker = train_data$predicted_risk,
                     cause = 1,
                     weighting = "marginal",
                     times = 3,
                     ROC = TRUE)
roc_5year <- timeROC(T = train_data$OS12,
                     delta = train_data$OS_event,
                     marker = train_data$predicted_risk,
                     cause = 1,
                     weighting = "marginal",
                     times = 5,
                     ROC = TRUE)
roc_data <- data.frame(
  FP = c(roc_3year$FP, roc_5year$FP),
  TP = c(roc_3year$TP, roc_5year$TP),
  Time = factor(rep(c("3-Year", "5-Year"), 
                    each = length(roc_3year$FP))))

roc_3year_test <- timeROC(T = test_data$OS12,
                          delta = test_data$OS_event,
                          marker = test_data$predicted_risk,
                          cause = 1,
                          weighting = "marginal",
                          times = 3,
                          ROC = TRUE)
roc_5year_test <- timeROC(T = test_data$OS12,
                          delta = test_data$OS_event,
                          marker = test_data$predicted_risk,
                          cause = 1,
                          weighting = "marginal",
                          times = 5,
                          ROC = TRUE)
roc_data_test <- data.frame(
  FP = c(roc_3year_test$FP, roc_5year_test$FP),
  TP = c(roc_3year_test$TP, roc_5year_test$TP),
  Time = factor(rep(c("3-Year", "5-Year"), 
                    each = length(roc_3year_test$FP))))
# 定义计算 AUC 的函数
compute_auc <- function(data, indices, time_point) {
  d <- data[indices, ]  # 自助法抽样
  roc <- timeROC(
    T = d$OS12,
    delta = d$OS_event,
    marker = d$predicted_risk,
    cause = 1,
    weighting = "marginal",
    times = time_point,
    ROC = TRUE
  )
  return(roc$AUC[2])  # 返回 AUC 值
}
library(boot)

# 计算训练集 AUC 的 95% CI
set.seed(123)  # 确保结果可重复

boot_3year_train <- boot(data = train_data, statistic = compute_auc, R = 1000, time_point = 3)
boot_5year_train <- boot(data = train_data, statistic = compute_auc, R = 1000, time_point = 5)

# 计算测试集 AUC 的 95% CI

boot_3year_test <- boot(data = test_data, statistic = compute_auc, R = 1000, time_point = 3)
boot_5year_test <- boot(data = test_data, statistic = compute_auc, R = 1000, time_point = 5)

# 训练集 AUC 及其 95% CI

auc_3year_train <- boot.ci(boot_3year_train, type = "perc")$percent[4:5]
auc_5year_train <- boot.ci(boot_5year_train, type = "perc")$percent[4:5]

# 测试集 AUC 及其 95% CI

auc_3year_test <- boot.ci(boot_3year_test, type = "perc")$percent[4:5]
auc_5year_test <- boot.ci(boot_5year_test, type = "perc")$percent[4:5]

cat("Training Set:\n")

cat(sprintf("3-Year AUC: %.3f (95%% CI: %.3f-%.3f)\n", roc_3year$AUC[2], auc_3year_train[1], auc_3year_train[2]))
cat(sprintf("5-Year AUC: %.3f (95%% CI: %.3f-%.3f)\n", roc_5year$AUC[2], auc_5year_train[1], auc_5year_train[2]))

cat("\nTest Set:\n")

cat(sprintf("3-Year AUC: %.3f (95%% CI: %.3f-%.3f)\n", roc_3year_test$AUC[2], auc_3year_test[1], auc_3year_test[2]))
cat(sprintf("5-Year AUC: %.3f (95%% CI: %.3f-%.3f)\n", roc_5year_test$AUC[2], auc_5year_test[1], auc_5year_test[2]))

# 设置图形参数
par(mfrow = c(1, 1))
par(mar = c(5, 5, 4, 2) + 0.1)  # 调整边距
plot(NA, xlim = c(0, 1), ylim = c(0, 1), 
     main = "Training cohort",
     xlab = "1 - Specificity", 
     ylab = "Sensitivity",
     cex.lab = 1.2, cex.main = 1.3, cex.axis = 1.1)

# 添加对角线参考线
abline(a = 0, b = 1, lty = 2, col = "gray")

# 绘制 ROC 曲线
lines(roc_3year$FP, roc_3year$TP, col = "#377EB8", lwd = 2)  # 3-year (蓝色)
lines(roc_5year$FP, roc_5year$TP, col = "#4DAF4A", lwd = 2)  # 5-year (绿色)

# 添加图例
legend(0.35,0.3, 
       legend = c(
         
         sprintf("3-Year (AUC = %.3f, 95%% CI: %.3f-%.3f)", roc_3year$AUC[2], auc_3year_train[1], auc_3year_train[2]),
         sprintf("5-Year (AUC = %.3f, 95%% CI: %.3f-%.3f)", roc_5year$AUC[2], auc_5year_train[1], auc_5year_train[2])
       ),
       col = c( "#377EB8", "#4DAF4A"), 
       lwd = 2, cex = 1, bty = "n")  
# 设置图形参数
par(mar = c(5, 5, 4, 2) + 0.1)  # 调整边距
plot(NA, xlim = c(0, 1), ylim = c(0, 1), 
     main = "Validation cohort",
     xlab = "1 - Specificity", 
     ylab = "Sensitivity",
     cex.lab = 1.2, cex.main = 1.3, cex.axis = 1.1)

# 添加对角线参考线
abline(a = 0, b = 1, lty = 2, col = "gray")

# 绘制 ROC 曲线

lines(roc_3year_test$FP, roc_3year_test$TP, col = "#377EB8", lwd = 2)  # 3-year (蓝色)
lines(roc_5year_test$FP, roc_5year_test$TP, col = "#4DAF4A", lwd = 2)  # 5-year (绿色)

# 添加图例
legend(0.35,0.3, 
       legend = c(
         
         sprintf("3-Year (AUC = %.3f, 95%% CI: %.3f-%.3f)", roc_3year_test$AUC[2], auc_3year_test[1], auc_3year_test[2]),
         sprintf("5-Year (AUC = %.3f, 95%% CI: %.3f-%.3f)", roc_5year_test$AUC[2], auc_5year_test[1], auc_5year_test[2])
       ),
       col = c("#377EB8", "#4DAF4A"), 
       lwd = 2, cex = 1, bty = "n")  



# 计算时间依赖性ROC
# 定义时间范围（这里使用训练集和测试集中最小到最大时间）
# 找到两个数据集中有足够事件的最长时间点
# 定义计算AUC的函数（保持不变）
compute_auc <- function(data, indices, time_point) {
  d <- data[indices, ]
  roc <- timeROC(T = d$OS, 
                 delta = d$OS_event,
                 marker = d$predicted_risk,
                 cause = 1,
                 times = time_point,
                 ROC = TRUE)
  roc$AUC[2]  # 返回对应时间点的AUC值
}

# 设置随机种子保证结果可重复
set.seed(123)
max_valid_time <- min(
  max(train_data$OS[train_data$OS_event == 1]),
  max(test_data$OS[test_data$OS_event == 1])
)

# 重新定义时间序列（每月一个点）
all_times <- seq(36, 60, by = 1)
# 计算带置信区间的ROC
# 初始化结果存储
train_results <- list()
test_results <- list()

# 对训练集进行Bootstrap计算（每年）
for(year in all_times) {
  cat("Processing training set for year", year, "\n")
  boot_res <- boot(data = train_data, statistic = compute_auc, R = 1000, time_point = year)
  train_results[[as.character(year)]] <- list(
    auc = timeROC(T = train_data$OS, delta = train_data$OS_event,
                  marker = train_data$predicted_risk, cause = 1,
                  times = year, ROC = TRUE)$AUC[2],
    ci = boot.ci(boot_res, type = "perc")$percent[4:5]
  )
}


# 对测试集进行Bootstrap计算（每年）
for(year in all_times) {
  cat("Processing test set for year", year, "\n")
  boot_res <- boot(data = test_data, statistic = compute_auc, R = 1000, time_point = year)
  test_results[[as.character(year)]] <- list(
    auc = timeROC(T = test_data$OS, delta = test_data$OS_event,
                  marker = test_data$predicted_risk, cause = 1,
                  times = year, ROC = TRUE)$AUC[2],
    ci = boot.ci(boot_res, type = "perc")$percent[4:5]
  )
}

train_df <- do.call(rbind, lapply(names(train_results), function(m) {
  data.frame(Month = as.numeric(m),  # 转换为月
             Dataset = "Training",
             AUC = train_results[[m]]$auc,
             CI_lower = train_results[[m]]$ci[1],
             CI_upper = train_results[[m]]$ci[2])
}))
test_df <- do.call(rbind, lapply(names(test_results), function(m) {
  data.frame(Month = as.numeric(m),  # 转换为月
             Dataset = "Validation",
             AUC = test_results[[m]]$auc,
             CI_lower = test_results[[m]]$ci[1],
             CI_upper = test_results[[m]]$ci[2])
}))
# 合并结果并移除NA值
all_results <- na.omit(rbind(train_df, test_df))
# 绘制曲线
library(ggplot2)
ggplot(all_results, aes(x = Month, y = AUC, color = Dataset)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  geom_ribbon(aes(ymin = CI_lower, ymax = CI_upper, fill = Dataset), 
              alpha = 0.2, linetype = 0) +
  scale_x_continuous(breaks = seq(0, max(all_results$Month), by = 1)) +  # 每年一个刻度
  labs(x = "Time after diagnosis (months)", 
       y = "Time-dependent AUC",
       color = "Dataset",
       fill = "Dataset") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5)) +
  ylim(0.5, 1) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray")  # 参考线



##############################绘制生存曲线
library(survminer)
# 自定义函数：计算指定组间的log-rank p值
pairwise_survdiff <- function(surv_formula, data, reference_group) {
  groups <- levels(data$risk_group)
  other_groups <- setdiff(groups, reference_group)
  
  results <- list()
  for (group in other_groups) {
    subset_data <- data[data$risk_group %in% c(reference_group, group), ]
    test <- survdiff(surv_formula, data = subset_data)
    pval <- 1 - pchisq(test$chisq, length(test$n) - 1)
    results[[paste(reference_group, "vs", group)]] <- pval
  }
  return(results)
}

train_data$risk_score <- predict(cox_model, newdata=train_data, type="lp")
test_data$risk_score <- predict(cox_model, newdata=test_data, type="lp")

combo_data <- expand.grid(
  Sgm    = c(0, 1),
  CP     = c(0, 1),
  AFP    = c(0, 1),
  TN     = c(0, 1),
  Gender = c(0, 1),
  MVI    = c(0, 1)
)

## 2. 用 Cox 模型计算每种组合对应的 risk_score（centered lp）
combo_data$risk_score <- predict(
  cox_model,
  newdata = combo_data,
  type = "lp"
)

combo_data$risk_group <- with(
  combo_data,
  ifelse(
    risk_score < 0.759187, "Low risk",
    ifelse(risk_score > 1.0252038, "High risk", "Intermediate risk")
  )
)

coef_values <- c(
  Sgm    = -0.641,
  CP     =  0.891,
  AFP    =  0.603,
  TN     =  0.681,
  Gender =  0.625,
  MVI    =  0.865
)

combo_data$risk_score_raw_manual <-
  coef_values["Sgm"]    * combo_data$Sgm +
  coef_values["CP"]     * combo_data$CP +
  coef_values["AFP"]    * combo_data$AFP +
  coef_values["TN"]     * combo_data$TN +
  coef_values["Gender"] * combo_data$Gender +
  coef_values["MVI"]    * combo_data$MVI



# 假设你的生存时间和事件列名为 "time" 和 "status"
res.cut <- surv_cutpoint(
  train_data,
  time = "OS_time",       
  event = "OS_event",     
  variables = "risk_score"
)
cut1 <- res.cut$cutpoint$cutpoint

train_data_high <- train_data[train_data$risk_score > cut1, ]  # 筛选出中+高组

res.cut2 <- surv_cutpoint(
  train_data_high,
  time = "OS_time",
  event = "OS_event",
  variables = "risk_score"
)
# 查看第二个截断点
cut2 <- res.cut2$cutpoint$cutpoint
min(train_data$risk_score)
max(train_data$risk_score)

train_data$risk_group <- cut(train_data$risk_score, 
                             breaks=c(-1.5,0.759187,1.0252038,2.5), 
                             labels=c("Low", "Intermediate", "High"),
                             include.lowest=TRUE)


# OS curves for training data
os_fit_train <- survfit(Surv(OS12, OS_event) ~ risk_group, data=train_data)
# 计算训练集组间比较p值（Low vs Medium, Low vs High）
train_pvals <- pairwise_survdiff(Surv(OS12, OS_event) ~ risk_group, 
                                 data = train_data, 
                                 reference_group = "Low")

train_plot <- ggsurvplot(os_fit_train, 
                         data = train_data,
                         palette = c("darkgreen", "darkblue", "darkred"),
                         title = "Nomogram risk stratification (training cohort)",
                         xlab = "Time after diagnosis (years)",
                         ylab = "Overall survival rate (%)",
                         legend.title = "Risk Group",
                         legend.labs = levels(train_data$risk_group),
                         risk.table = TRUE,
                         break.x.by = 1,
                         xlim = c(0,10),
                         pval = FALSE)


train_plot$plot <- train_plot$plot + 
  annotate("text", x = 1, y = 0.22, 
           label = paste0("Log-rank tset: P < 0.001"),
           color = "black") +
  annotate("text", x = 1, y = 0.18, 
           label = paste0("Cox Hazard Ratio model (95%CI):"),
           color = "black") +
  annotate("text", x = 1, y = 0.14, 
           label = paste0("Low risk vs Intermediate risk: P < 0.001"),
           color = "darkblue") +
  annotate("text", x = 1, y = 0.1, 
           label = paste0("Low risk vs High risk: P < 0.001 "),
           color = "darkred")
print(train_plot)
####
quantiles <- quantile(test_data$risk_score, probs = c(0.7, 0.9))

test_data$risk_group <- cut(test_data$risk_score, 
                            breaks=c(-5,0.1439687,0.7850841,5), 
                            labels=c("Low", "Intermediate", "High"),
                            include.lowest=TRUE)

os_fit_test <- survfit(Surv(OS12, OS_event) ~ risk_group, data=test_data)
test_pvals <- pairwise_survdiff(Surv(OS12, OS_event) ~ risk_group, 
                                data = test_data, 
                                reference_group = "Low")
test_plot <- ggsurvplot(
  os_fit_test, 
  data = test_data,
  palette = c("darkgreen", "darkblue", "darkred"),
  title = "Nomogram risk stratification (validation cohort)",
  xlab = "Time after diagnosis (years)",
  ylab = "Overall survival rate (%)",
  legend.title = "Risk Group",
  legend.labs = levels(test_data$risk_group),
  risk.table = TRUE,
  break.x.by = 1,
  xlim = c(0,10),
  pval = FALSE
)
test_plot$plot <- test_plot$plot + 
  annotate("text", x = 1, y = 0.22, 
           label = paste0("Log-rank tset: P < 0.001"),
           color = "black") +
  annotate("text", x = 1, y = 0.18, 
           label = paste0("Cox Hazard Ratio model (95%CI):"),
           color = "black") +
  annotate("text", x = 1, y = 0.14, 
           label = paste0("Low risk vs Intermediate risk: P < 0.001"),
           color = "darkblue") +
  annotate("text", x = 1, y = 0.1, 
           label = paste0("Low risk vs High risk: P < 0.001 "),
           color = "darkred")
print(test_plot)

##############################指数计算
library(survival)
library(risksetROC)
library(nricens)
library(pec)
library(compareC)
library(survIDINRI)
# 准备数据：合并预测概率和真实生存数据
# 训练集
train_compare_cp <- data.frame(
  time = train_data$OS12,
  status = train_data$OS_event,
  nomogram_3year = 1-pred_3year,
  nomogram_5year = 1-pred_5year,
  CPAB_3year = train_data$pred_cpab_km_train_3,
  CPAB_5year = train_data$pred_cpab_km_train_5
)

train_compare_bclc <- data.frame(
  time = train_data$OS12,
  status = train_data$OS_event,
  nomogram_3year = 1-pred_3year,
  nomogram_5year = 1-pred_5year,
  
  BCLC_3year = train_data$pred_bclc_km_train_3,
  BCLC_5year = train_data$pred_bclc_km_train_5
)
# 测试集
test_compare_cp <- data.frame(
  time = test_data$OS12,
  status = test_data$OS_event,
  nomogram_3year = 1 - pred_3year_test,
  nomogram_5year = 1 - pred_5year_test,
  CPAB_3year = test_data$pred_cpab_km_test_3,
  CPAB_5year = test_data$pred_cpab_km_test_5
)
test_compare_bclc <- data.frame(
  time = test_data$OS12,
  status = test_data$OS_event,
  nomogram_3year = 1 - pred_3year_test,
  nomogram_5year = 1 - pred_5year_test,
  BCLC_3year = test_data$pred_bclc_km_test_3,
  BCLC_5year = test_data$pred_bclc_km_test_5
)

# 计算训练集的NRI
train_nri_3year_cp <- nribin(
  event = train_compare_cp$status,
  p.std = train_compare_cp$CPAB_3year, # 旧模型(CPS)的预测概率
  p.new = train_compare_cp$nomogram_3year,  # 新模型(nomogram)的预测概率
  cut = c(0.2,0.4),  # 风险分类的截断值(可根据实际情况调整)
  niter = 1000  # bootstrap迭代次数
)

train_nri_3year_bclc <- nribin(
  event = train_compare_bclc$status,
  p.std = train_compare_bclc$BCLC_3year,  # 旧模型(CPS)的预测概率
  p.new = train_compare_bclc$nomogram_3year,  # 新模型(nomogram)的预测概率
  cut = c(0.2,0.4),  # 风险分类的截断值(可根据实际情况调整)
  niter = 1000  # bootstrap迭代次数
)

train_nri_5year_cp <- nribin(
  event = train_compare_cp$status,
  p.std = train_compare_cp$CPAB_5year,  # 旧模型(CPAB)的预测概率
  p.new = train_compare_cp$nomogram_5year,  # 新模型(nomogram)的预测概率
  cut = c(0.2, 0.4),  # 风险分类的截断值
  niter = 1000  # bootstrap迭代次数
)
train_nri_5year_bclc <- nribin(
  event = train_compare_bclc$status,
  p.std = train_compare_bclc$BCLC_5year,  # 旧模型(CPS)的预测概率
  p.new = train_compare_bclc$nomogram_5year,  # 新模型(nomogram)的预测概率
  cut = c(0.2,0.4),  # 风险分类的截断值(可根据实际情况调整)
  niter = 1000  # bootstrap迭代次数
)

test_nri_3year_cp <- nribin(
  event = test_compare_cp$status,
  p.std = test_compare_cp$CPAB_3year,  # 旧模型(CPAB)的预测概率
  p.new = test_compare_cp$nomogram_3year,  # 新模型(nomogram)的预测概率
  cut = c(0.2, 0.4),  # 风险分类的截断值
  niter = 1000  # bootstrap迭代次数
)
test_nri_3year_bclc <- nribin(
  event = test_compare_bclc$status,
  p.std = test_compare_bclc$BCLC_3year,  # 旧模型(CPS)的预测概率
  p.new = test_compare_bclc$nomogram_3year,  # 新模型(nomogram)的预测概率
  cut = c(0.2,0.4),  # 风险分类的截断值(可根据实际情况调整)
  niter = 1000  # bootstrap迭代次数
)

test_nri_5year_cp <- nribin(
  event = test_compare_cp$status,
  p.std = test_compare_cp$CPAB_5year,  # 旧模型(CPAB)的预测概率
  p.new = test_compare_cp$nomogram_5year,  # 新模型(nomogram)的预测概率
  cut = c(0.2, 0.4),  # 风险分类的截断值
  niter = 1000  # bootstrap迭代次数
)
test_nri_5year_bclc <- nribin(
  event = test_compare_bclc$status,
  p.std = test_compare_bclc$BCLC_5year,  # 旧模型(CPS)的预测概率
  p.new = test_compare_bclc$nomogram_5year,  # 新模型(nomogram)的预测概率
  cut = c(0.2,0.4),  # 风险分类的截断值(可根据实际情况调整)
  niter = 1000  # bootstrap迭代次数
)
###############IDI
# 自定义IDI计算函数
calculate_idi_with_ci <- function(time, status, old_pred, new_pred, cutoff_time, n_boot = 1000) {
  # 创建事件指示变量（在cutoff_time前发生事件）
  event_indicator <- ifelse(time <= cutoff_time & status == 1, 1, 0)
  
  # 事件组和非事件组的索引
  events <- which(event_indicator == 1)
  nonevents <- which(event_indicator == 0)
  
  # 计算各组平均预测概率
  P_old_events <- mean(old_pred[events])
  P_old_nonevents <- mean(old_pred[nonevents])
  P_new_events <- mean(new_pred[events])
  P_new_nonevents <- mean(new_pred[nonevents])
  
  # 计算IDI
  idi <- (P_new_events - P_new_nonevents) - (P_old_events - P_old_nonevents)
  
  # 计算标准误和95%CI（使用bootstrap）
  boot_idi <- numeric(n_boot)
  n <- length(time)
  
  for(i in 1:n_boot) {
    idx <- sample(1:n, n, replace = TRUE)
    boot_events <- which(time[idx] <= cutoff_time & status[idx] == 1)
    boot_nonevents <- which(!(time[idx] <= cutoff_time & status[idx] == 1))
    
    if(length(boot_events) > 0 & length(boot_nonevents) > 0) {
      P_old_events_boot <- mean(old_pred[idx][boot_events])
      P_old_nonevents_boot <- mean(old_pred[idx][boot_nonevents])
      P_new_events_boot <- mean(new_pred[idx][boot_events])
      P_new_nonevents_boot <- mean(new_pred[idx][boot_nonevents])
      
      boot_idi[i] <- (P_new_events_boot - P_new_nonevents_boot) - 
        (P_old_events_boot - P_old_nonevents_boot)
    }
  }
  
  # 计算统计量
  se <- sd(boot_idi, na.rm = TRUE)
  z <- idi / se
  p_value <- 2 * pnorm(-abs(z))
  
  # 计算95% CI (正态近似法)
  ci_lower <- idi - 1.96 * se
  ci_upper <- idi + 1.96 * se
  
  # 计算95% CI (百分位数法)
  boot_idi <- boot_idi[!is.na(boot_idi)]
  ci_percentile <- quantile(boot_idi, probs = c(0.025, 0.975), na.rm = TRUE)
  
  # 返回结果
  list(
    IDI = idi,
    SE = se,
    Z = z,
    P_value = p_value,
    CI_normal = c(ci_lower, ci_upper),
    CI_percentile = ci_percentile,
    P_old_events = P_old_events,
    P_old_nonevents = P_old_nonevents,
    P_new_events = P_new_events,
    P_new_nonevents = P_new_nonevents,
    boot_samples = boot_idi  # 保存bootstrap样本用于绘图等
  )
}

# 计算3年IDI（训练集）
train_idi_3year <- calculate_idi_with_ci(
  time = train_compare_cp$time,
  status = train_compare_cp$status,
  old_pred = train_compare_cp$CPAB_3year,
  new_pred = train_compare_cp$nomogram_3year,
  cutoff_time = 3
)

test_idi_3year <- calculate_idi_with_ci(
  time = test_compare_cp$time,
  status = test_compare_cp$status,
  old_pred = test_compare_cp$CPAB_3year,
  new_pred = test_compare_cp$nomogram_3year,
  cutoff_time = 3
)

train_idi_5year <- calculate_idi_with_ci(
  time = train_compare_cp$time,
  status = train_compare_cp$status,
  old_pred = train_compare_cp$CPAB_3year,
  new_pred = train_compare_cp$nomogram_5year,
  cutoff_time = 5
)

test_idi_5year <- calculate_idi_with_ci(
  time = test_compare_cp$time,
  status = test_compare_cp$status,
  old_pred = test_compare_cp$CPAB_5year,
  new_pred = test_compare_cp$nomogram_5year,
  cutoff_time = 5
)

train_idi_3year_bclc <- calculate_idi_with_ci(
  time = train_compare_bclc$time,
  status = train_compare_bclc$status,
  old_pred = train_compare_bclc$BCLC_3year,
  new_pred = train_compare_bclc$nomogram_3year,
  cutoff_time = 3
)
train_idi_5year_bclc <- calculate_idi_with_ci(
  time = train_compare_bclc$time,
  status = train_compare_bclc$status,
  old_pred = train_compare_bclc$BCLC_5year,
  new_pred = train_compare_bclc$nomogram_5year,
  cutoff_time = 5
)

test_idi_3year_bclc <- calculate_idi_with_ci(
  time = test_compare_bclc$time,
  status = test_compare_bclc$status,
  old_pred = test_compare_bclc$BCLC_3year,
  new_pred = test_compare_bclc$nomogram_3year,
  cutoff_time = 3
)
test_idi_5year_bclc <- calculate_idi_with_ci(
  time = test_compare_bclc$time,
  status = test_compare_bclc$status,
  old_pred = test_compare_bclc$BCLC_5year,
  new_pred = test_compare_bclc$nomogram_5year,
  cutoff_time = 5
)

########################C-index

# 比较两个模型的C-index
cph_old_cpab <- coxph(Surv(time, status) ~ CPAB_3year, data = train_compare_cp)
cph_new_cpab <- coxph(Surv(time, status) ~ nomogram_3year, data = train_compare_cp)

cph_old_bclc <- coxph(Surv(time, status) ~ BCLC_3year, data = train_compare_bclc)
cph_new_bclc <- coxph(Surv(time, status) ~ nomogram_3year, data = train_compare_bclc)

# 提取C-index和标准误
c_old_cpab <- summary(cph_old_cpab)$concordance
c_new_cpab <- summary(cph_new_cpab)$concordance
c_old_bclc <- summary(cph_old_bclc)$concordance
c_new_bclc <- summary(cph_new_bclc)$concordance


# 计算差异
c_diff_cpab <- c_new_cpab[1] - c_old_cpab[1]
se_diff_cpab <- sqrt(c_old_cpab[2]^2 + c_new_cpab[2]^2)
c_diff_bclc <- c_new_bclc[1] - c_old_bclc[1]
se_diff_bclc <- sqrt(c_old_bclc[2]^2 + c_new_bclc[2]^2)

# 计算95% CI和p值
ci_lower_cpab <- c_diff_cpab - 1.96 * se_diff_cpab
ci_upper_cpab <- c_diff_cpab + 1.96 * se_diff_cpab
z_score_cpab <- c_diff_cpab / se_diff_cpab
p_value_cpab <- 2 * pnorm(-abs(z_score_cpab))

ci_lower_bclc <- c_diff_bclc - 1.96 * se_diff_bclc
ci_upper_bclc <- c_diff_bclc + 1.96 * se_diff_bclc
z_score_bclc <- c_diff_bclc / se_diff_bclc
p_value_bclc <- 2 * pnorm(-abs(z_score_bclc))


# 输出结果
cat("=== C-index差异比较 (3年) ===\n\n")

cat("Nomogram vs CPAB:\n")
cat(sprintf("C-index差异: %.4f (95%% CI: %.4f-%.4f)\n", c_diff_cpab, ci_lower_cpab, ci_upper_cpab))
cat(sprintf("P值: %.4f\n", p_value_cpab))
cat(sprintf("旧模型C-index: %.4f (SE: %.4f)\n", c_old_cpab[1], c_old_cpab[2]))
cat(sprintf("新模型C-index: %.4f (SE: %.4f)\n\n", c_new_cpab[1], c_new_cpab[2]))

cat("Nomogram vs BCLC:\n")
cat(sprintf("C-index差异: %.4f (95%% CI: %.4f-%.4f)\n", c_diff_bclc, ci_lower_bclc, ci_upper_bclc))
cat(sprintf("P值: %.4f\n", p_value_bclc))
cat(sprintf("旧模型C-index: %.4f (SE: %.4f)\n", c_old_bclc[1], c_old_bclc[2]))
cat(sprintf("新模型C-index: %.4f (SE: %.4f)\n\n", c_new_bclc[1], c_new_bclc[2]))

# 创建结果数据框
c_index_results <- data.frame(
  Comparison = c("Nomogram vs CPAB", "Nomogram vs BCLC"),
  C_index_diff = c(c_diff_cpab, c_diff_bclc),
  CI_lower = c(ci_lower_cpab, ci_lower_bclc),
  CI_upper = c(ci_upper_cpab, ci_upper_bclc),
  P_value = c(p_value_cpab, p_value_bclc),
  Old_C_index = c(c_old_cpab[1], c_old_bclc[1]),
  New_C_index = c(c_new_cpab[1], c_new_bclc[1]),
  Old_SE = c(c_old_cpab[2], c_old_bclc[2]),
  New_SE = c(c_new_cpab[2], c_new_bclc[2])
)

print(c_index_results)

# 可视化C-index差异
library(ggplot2)

ggplot(c_index_results, aes(x = Comparison, y = C_index_diff)) +
  geom_point(size = 3, color = "blue") +
  geom_errorbar(aes(ymin = CI_lower, ymax = CI_upper), width = 0.2, color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "C-index差异比较 (Nomogram vs 传统评分系统)",
       x = "比较组",
       y = "C-index差异 (新模型 - 旧模型)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# 测试集C-index计算
# 比较两个模型的C-index（测试集）
cph_old_cpab <- coxph(Surv(time, status) ~ CPAB_3year, data = test_compare_cp)
cph_new_cpab <- coxph(Surv(time, status) ~ nomogram_3year, data = test_compare_cp)

cph_old_bclc <- coxph(Surv(time, status) ~ BCLC_3year, data = test_compare_bclc)
cph_new_bclc <- coxph(Surv(time, status) ~ nomogram_3year, data = test_compare_bclc)

# 提取C-index和标准误
c_old_cpab <- summary(cph_old_cpab)$concordance
c_new_cpab <- summary(cph_new_cpab)$concordance
c_old_bclc <- summary(cph_old_bclc)$concordance
c_new_bclc <- summary(cph_new_bclc)$concordance

# 计算差异
c_diff_cpab <- c_new_cpab[1] - c_old_cpab[1]
se_diff_cpab <- sqrt(c_old_cpab[2]^2 + c_new_cpab[2]^2)
c_diff_bclc <- c_new_bclc[1] - c_old_bclc[1]
se_diff_bclc <- sqrt(c_old_bclc[2]^2 + c_new_bclc[2]^2)

# 计算95% CI和p值
ci_lower_cpab <- c_diff_cpab - 1.96 * se_diff_cpab
ci_upper_cpab <- c_diff_cpab + 1.96 * se_diff_cpab
z_score_cpab <- c_diff_cpab / se_diff_cpab
p_value_cpab <- 2 * pnorm(-abs(z_score_cpab))

ci_lower_bclc <- c_diff_bclc - 1.96 * se_diff_bclc
ci_upper_bclc <- c_diff_bclc + 1.96 * se_diff_bclc
z_score_bclc <- c_diff_bclc / se_diff_bclc
p_value_bclc <- 2 * pnorm(-abs(z_score_bclc))

# 输出结果80
cat("=== Test Set C-index差异比较 (3年) ===\n\n")

cat("Nomogram vs CPAB:\n")
cat(sprintf("C-index差异: %.4f (95%% CI: %.4f-%.4f)\n", c_diff_cpab, ci_lower_cpab, ci_upper_cpab))
cat(sprintf("P值: %.4f\n", p_value_cpab))
cat(sprintf("旧模型C-index: %.4f (SE: %.4f)\n", c_old_cpab[1], c_old_cpab[2]))
cat(sprintf("新模型C-index: %.4f (SE: %.4f)\n\n", c_new_cpab[1], c_new_cpab[2]))

cat("Nomogram vs BCLC:\n")
cat(sprintf("C-index差异: %.4f (95%% CI: %.4f-%.4f)\n", c_diff_bclc, ci_lower_bclc, ci_upper_bclc))
cat(sprintf("P值: %.4f\n", p_value_bclc))
cat(sprintf("旧模型C-index: %.4f (SE: %.4f)\n", c_old_bclc[1], c_old_bclc[2]))
cat(sprintf("新模型C-index: %.4f (SE: %.4f)\n\n", c_new_bclc[1], c_new_bclc[2]))

# 创建结果数据框
c_index_results_test <- data.frame(
  Comparison = c("Nomogram vs CPAB", "Nomogram vs BCLC"),
  C_index_diff = c(c_diff_cpab, c_diff_bclc),
  CI_lower = c(ci_lower_cpab, ci_lower_bclc),
  CI_upper = c(ci_upper_cpab, ci_upper_bclc),
  P_value = c(p_value_cpab, p_value_bclc),
  Old_C_index = c(c_old_cpab[1], c_old_bclc[1]),
  New_C_index = c(c_new_cpab[1], c_new_bclc[1]),
  Old_SE = c(c_old_cpab[2], c_old_bclc[2]),
  New_SE = c(c_new_cpab[2], c_new_bclc[2])
)
print(c_index_results_test)

ggplot(c_index_results_test, aes(x = Comparison, y = C_index_diff)) +
  geom_point(size = 3, color = "red") +
  geom_errorbar(aes(ymin = CI_lower, ymax = CI_upper), width = 0.2, color = "red") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "blue") +
  labs(title = "Test Set C-index差异比较 (Nomogram vs 传统评分系统)",
       x = "比较组",
       y = "C-index差异 (新模型 - 旧模型)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 可选：同时显示训练集和测试集结果进行比较
# 如果您已经计算了训练集的结果，可以合并显示
if (exists("c_index_results")) {
  c_index_results$Dataset <- "Train"
  c_index_results_test$Dataset <- "Test"
  combined_results <- rbind(c_index_results, c_index_results_test)
  
  ggplot(combined_results, aes(x = Comparison, y = C_index_diff, color = Dataset)) +
    geom_point(position = position_dodge(width = 0.5), size = 3) +
    geom_errorbar(aes(ymin = CI_lower, ymax = CI_upper), 
                  position = position_dodge(width = 0.5), width = 0.2) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
    labs(title = "Train vs Validation",
         x = "Groups",
         y = "C-index (95% CI)",
         color = "Dataset") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_color_manual(values = c("Train" = "blue", "Test" = "red"))
}
