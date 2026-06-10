# ============================================
# R语言：缺失值比例可视化
# ============================================

# 1. 加载包
library(readxl)    # 读取xls文件
library(dplyr)     # 数据处理
library(tidyr)     # 数据转换
library(ggplot2)   # 可视化
library(gridExtra) # 组合图形

# 2. 读取数据
train_path <- "C:/data/rdata/mlm/train_os.xls"
test_path  <- "C:/data/rdata/mlm/test_os.xls"

train_raw <- read_excel(train_path, na = "NA")
test_raw  <- read_excel(test_path, na = "NA")

# 3. 计算缺失值比例函数
calc_missing_prop <- function(df, dataset_name) {
  miss_prop <- data.frame(
    variable = names(df),
    n_missing = sapply(df, function(x) sum(is.na(x))),
    n_total = nrow(df),
    stringsAsFactors = FALSE
  ) %>%
    mutate(
      missing_rate = round(n_missing / n_total * 100, 2),
      dataset = dataset_name
    ) %>%
    filter(n_missing > 0) %>%           # 只保留有缺失值的变量
    arrange(desc(missing_rate))
  
  return(miss_prop)
}

train_miss <- calc_missing_prop(train_raw, "Training Set")
test_miss  <- calc_missing_prop(test_raw, "Validation Set")

# 4. 合并数据用于分面显示
all_miss <- bind_rows(train_miss, test_miss)

# 5. 定义颜色（根据缺失率分级）
get_color <- function(rate) {
  ifelse(rate > 15, "#E74C3C",      # 红色：高缺失率
         ifelse(rate > 5, "#F39C12", # 橙色：中等缺失率
                "#3498DB"))          # 蓝色：低缺失率
}

# 6. 绘制对比图（分面）
p1 <- ggplot(all_miss, aes(x = reorder(variable, missing_rate), 
                           y = missing_rate, 
                           fill = factor(ifelse(missing_rate > 15, "High (>15%)",
                                                ifelse(missing_rate > 5, "Medium (5-15%)", "Low (<5%)"))))) +
  geom_bar(stat = "identity", width = 0.7, color = "white") +
  geom_text(aes(label = paste0(missing_rate, "%")), 
            hjust = -0.1, size = 3) +
  coord_flip() +
  scale_fill_manual(values = c("Low (<5%)" = "#3498DB", 
                               "Medium (5-15%)" = "#F39C12", 
                               "High (>15%)" = "#E74C3C"),
                    name = "Missing Level") +
  facet_wrap(~ dataset, scales = "free_y") +
  labs(title = "Missing Data Proportion by Variable",
       subtitle = "Training Set vs Validation Set",
       x = "Variable", 
       y = "Missing Rate (%)") +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    strip.text = element_text(face = "bold", size = 12),
    legend.position = "bottom",
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  geom_hline(data = data.frame(dataset = c("Training Set", "Validation Set"),
                               threshold = c(5, 5)),
             aes(yintercept = threshold), linetype = "dashed", 
             color = "gray50", alpha = 0.7) +
  geom_hline(data = data.frame(dataset = c("Training Set", "Validation Set"),
                               threshold = c(15, 15)),
             aes(yintercept = threshold), linetype = "dashed", 
             color = "red", alpha = 0.5) +
  expand_limits(y = max(all_miss$missing_rate) * 1.1)

print(p1)

# 保存图片
ggsave("C:/data/rdata/mlm/missing_proportion_comparison.png", 
       plot = p1, width = 14, height = 10, dpi = 300)