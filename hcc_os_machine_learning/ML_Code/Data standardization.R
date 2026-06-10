# ============================================================
# 计算连续变量的均值和标准差（Z-score标准化参数）
# 并保存为 RDS 文件
# ============================================================

# 1. 读取训练集（原始值，未标准化）
train_df <- read.csv("C:/data/rdata/mlm/train_os_cart_imputed.csv", stringsAsFactors = FALSE)

# 2. 指定需要标准化的连续变量
continuous_vars <- c("tumor_num", "tumor_dia", "nlr", "glb", "bun", "copy")

# 3. 计算均值和标准差
scaling_params <- list()
for (var in continuous_vars) {
  x <- train_df[[var]]
  mn <- mean(x, na.rm = TRUE)
  sd_val <- sd(x, na.rm = TRUE)
  if (is.na(sd_val) || sd_val == 0) sd_val <- 1   # 处理零标准差
  scaling_params[[var]] <- list(mean = mn, sd = sd_val)
}

# 4. 打印均值和标准差（显示）
cat("\n===== 连续变量的均值和标准差 =====\n")
for (var in names(scaling_params)) {
  cat(sprintf("%s: mean = %.4f, sd = %.4f\n", 
              var, 
              scaling_params[[var]]$mean, 
              scaling_params[[var]]$sd))
}

# 5. 保存为 RDS 文件
saveRDS(scaling_params, file = "C:/data/rdata/mlm/continuous_scaling_params.rds")
cat("\n标准化参数已保存至: C:/data/rdata/mlm/continuous_scaling_params.rds\n")