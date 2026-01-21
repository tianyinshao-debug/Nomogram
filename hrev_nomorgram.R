my_data <- readRDS("~/Desktop/vt_sgd_prediction_model.rds")

library(rms)
data <- my_data$model
ddist <- datadist(data)
options(datadist = "ddist")
fit <- lrm(hrev ~ vt + sgd, data = data)

# 绘制列线图
nom <- nomogram(fit, 
                fun = plogis, 
                fun.at = c(0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99),
                funlabel = "Predicted Probability",
                lp = TRUE)
plot(nom)


# 绘制带置信区间的列线图
nom_ci <- nomogram(fit,
                   fun = plogis,
                   fun.at = c(0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99),
                   funlabel = "Predicted Probability (95% CI)",
                   lp = TRUE,
                   conf.int = TRUE)

plot(nom_ci, xfrac=0.15)

# 绘制带系数的列线图
par(mfrow = c(1, 2), mar = c(4, 4, 4, 2))

# 左图：模型系数可视化
coefs <- coef(fit)[-1]  # 去掉截距项
names(coefs) <- c("vt", "sgd")
barplot(coefs, 
        main = "Model Coefficients",
        ylab = "Coefficient Value",
        col = c("skyblue", "lightgreen"),
        ylim = c(0, max(coefs) * 1.2))
abline(h = 0, lty = 2, col = "gray")
# 右图：列线图
nom_pub <- nomogram(fit,
                    fun = plogis,
                    fun.at = c(0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99),
                    funlabel = "Predicted Probability (95% CI)",
                    lp = TRUE,
                    conf.int = TRUE)
plot(nom_pub, xfrac = 0.2, cex.axis = 0.9, cex.var = 1.1)



