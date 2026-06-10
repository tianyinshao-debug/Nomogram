# =====================================================
# 基线表 + 生存分析：Development set vs External test set
# 输出总体、两组中位生存时间及 log-rank p 值
# 不显示缺失值，p值三位小数，导出 Excel
# =====================================================

# ------------------- 0. 安装并加载所需包 -------------------
required_pkgs <- c("dplyr", "gtsummary", "rstatix", "writexl", "survival")
for (pkg in required_pkgs) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

# ------------------- 1. 定义变量分类 -------------------
continuous_vars <- c("age", "bmi", "tumor_num", "tumor_dia", "portal_dia",
                     "splenn_dia", "hb", "wbc", "plt", "neu", "lym", "pt", 
                     "inr", "alt", "ast", "tb", "tp", "tc", "alb", "glb", 
                     "cr", "bun", "alp", "copy", "opt", "bleed", "zd_time", 
                     "fol_time", "hsp_day")

binary_vars <- c("sex", "sgm", "dbt", "hpv", "red_sign", "hr_ev", "pvb", 
                 "meld10", "mat16.3", "cupi_cl", "child_cl", "bclc", "pvt", "hepatitis", 
                 "anti_vir", "ag1.5", "afp400", "hpm", "mhp", "opt_trans", "rm", 
                 "mvi", "satellite", "cpsule", "S4", "sec_inj", "post_bld2", 
                 "post_bld3", "bil_fis3", "infection", "abd_eff2", "abd_eff3", 
                 "ple_eff2", "ple_eff3", "LF50", "yxss3", "ren_ins", "post_xhdcx", 
                 "opsi", "post_trans", "post_pvt", "ttrec", "fol_lfevent", "suv")   

multiclass_vars <- c("asa", "egv", "es", "vp", "non3", "rec_mod", "fl_tr", "dth_rs")

# ------------------- 2. 读取数据（自动处理编码）-------------------
dev_path <- "C:/data/rdata/mlm/train_data.csv"
ext_path <- "C:/data/rdata/mlm/test_data.csv"

read_safe_csv <- function(path) {
  encodings <- c("GBK", "GB18030", "UTF-8")
  for (enc in encodings) {
    tryCatch({
      df <- read.csv(path, na.strings = "NA", stringsAsFactors = FALSE, fileEncoding = enc)
      message("成功使用编码: ", enc, " 读取文件: ", basename(path))
      return(df)
    }, error = function(e) {})
  }
  warning("自动编码检测失败，使用系统默认编码，数据可能乱码: ", basename(path))
  read.csv(path, na.strings = "NA", stringsAsFactors = FALSE)
}

dev_data <- read_safe_csv(dev_path)
ext_data <- read_safe_csv(ext_path)

# ------------------- 3. 统一变量类型 -------------------
convert_data_types <- function(df) {
  for (v in continuous_vars) {
    if (v %in% names(df)) df[[v]] <- suppressWarnings(as.numeric(as.character(df[[v]])))
  }
  for (v in binary_vars) {
    if (v %in% names(df)) df[[v]] <- as.factor(df[[v]])
  }
  for (v in multiclass_vars) {
    if (v %in% names(df)) df[[v]] <- as.factor(df[[v]])
  }
  if ("os" %in% names(df)) df[["os"]] <- suppressWarnings(as.numeric(as.character(df[["os"]])))
  if ("suv" %in% names(df)) df[["suv"]] <- suppressWarnings(as.numeric(as.character(df[["suv"]])))
  return(df)
}

dev_data <- convert_data_types(dev_data)
ext_data <- convert_data_types(ext_data)

dev_data$group <- "Development set"
ext_data$group <- "External test set"
all_data <- dplyr::bind_rows(dev_data, ext_data)

# 检查生存变量
if (!"os" %in% names(all_data)) stop("数据中缺少生存时间变量 'os'")
if (!"suv" %in% names(all_data)) stop("数据中缺少生存状态变量 'suv'")

# ------------------- 4. 仅保留实际存在的变量 -------------------
all_vars <- c(continuous_vars, binary_vars, multiclass_vars)
existing_vars <- intersect(all_vars, names(all_data))
missing_vars <- setdiff(all_vars, existing_vars)
if (length(missing_vars) > 0) {
  warning("以下变量不在任何数据文件中，已忽略: ", paste(missing_vars, collapse = ", "))
}
continuous_vars <- intersect(continuous_vars, existing_vars)
binary_vars <- intersect(binary_vars, existing_vars)
multiclass_vars <- intersect(multiclass_vars, existing_vars)

# ------------------- 5. 正态性判断 -------------------
normality_flag <- list()
for (v in continuous_vars) {
  vec_clean <- all_data[[v]][!is.na(all_data[[v]])]
  if (length(vec_clean) >= 3) {
    p_val <- shapiro.test(vec_clean)$p.value
    normality_flag[[v]] <- ifelse(p_val >= 0.05, "normal", "nonnormal")
  } else {
    normality_flag[[v]] <- "nonnormal"
  }
}

normality_df <- data.frame(
  Variable = continuous_vars,
  Normality = ifelse(unlist(normality_flag) == "normal", "Normal (mean ± SD, t-test)", "Non-normal (median [IQR], Wilcoxon test)"),
  Shapiro_P_value = sapply(continuous_vars, function(v) {
    vec <- all_data[[v]][!is.na(all_data[[v]])]
    if (length(vec) >= 3) shapiro.test(vec)$p.value else NA
  })
)
normality_df$Shapiro_P_value <- round(normality_df$Shapiro_P_value, 4)

# ------------------- 6. 基线表描述统计与检验 -------------------
stat_list <- list()
for (v in continuous_vars) {
  stat_list[[v]] <- ifelse(normality_flag[[v]] == "normal", "{mean} ± {sd}", "{median} ({p25}, {p75})")
}
continuous_test <- list()
for (v in continuous_vars) {
  continuous_test[[v]] <- ifelse(normality_flag[[v]] == "normal", "t.test", "wilcox.test")
}

var_type <- list()
for (v in continuous_vars) var_type[[v]] <- "continuous"
for (v in binary_vars) var_type[[v]] <- "categorical"
for (v in multiclass_vars) var_type[[v]] <- "categorical"

pvalue_fun_3dec <- function(x) {
  formatted <- sprintf("%.3f", x)
  formatted <- ifelse(x < 0.001, "<0.001", formatted)
  return(formatted)
}

# 生成基线表（不显示缺失）
tbl1 <- all_data %>%
  dplyr::select(group, dplyr::all_of(c(continuous_vars, binary_vars, multiclass_vars))) %>%
  gtsummary::tbl_summary(
    by = group,
    statistic = stat_list,
    type = var_type,
    missing = "no",
    digits = list(gtsummary::all_continuous() ~ c(2, 2),
                  gtsummary::all_categorical() ~ c(0, 1))
  ) %>%
  gtsummary::add_p(
    test = continuous_test,
    test.args = gtsummary::all_tests("t.test") ~ list(var.equal = FALSE),
    pvalue_fun = pvalue_fun_3dec
  ) %>%
  gtsummary::add_overall() %>%
  gtsummary::modify_header(label = "**Variable**") %>%
  gtsummary::bold_labels()

# ------------------- 7. 生存分析（包含总体、两组）------------------
# 提取中位生存时间及95% CI 的函数（可指定数据集）
get_median_survival <- function(data) {
  fit <- survfit(Surv(os, suv) ~ 1, data = data)
  med <- summary(fit)$table["median"]
  lower <- summary(fit)$table["0.95LCL"]
  upper <- summary(fit)$table["0.95UCL"]
  if (is.na(med)) return("未达到")
  else return(sprintf("%.1f (%.1f-%.1f)", med, lower, upper))
}

# 总体中位生存时间
overall_med <- get_median_survival(all_data)

# 两组分别计算
dev_med <- get_median_survival(subset(all_data, group == "Development set"))
ext_med <- get_median_survival(subset(all_data, group == "External test set"))

# log-rank 检验（比较两组）
logrank_test <- survdiff(Surv(os, suv) ~ group, data = all_data)
p_logrank <- pchisq(logrank_test$chisq, df = 1, lower.tail = FALSE)
p_logrank_formatted <- ifelse(p_logrank < 0.001, "<0.001", sprintf("%.3f", p_logrank))

# ------------------- 8. 将基线表转为数据框并添加生存行（基于位置索引）------------------
tbl1_df <- as.data.frame(tbl1)
colnames(tbl1_df) <- gsub("\\*", "", colnames(tbl1_df))
colnames(tbl1_df) <- trimws(colnames(tbl1_df))

n_cols <- ncol(tbl1_df)

# 定位各列索引
overall_idx <- grep("overall", colnames(tbl1_df), ignore.case = TRUE)
if (length(overall_idx) == 0) overall_idx <- NA else overall_idx <- overall_idx[1]

dev_idx <- grep("development", colnames(tbl1_df), ignore.case = TRUE)[1]
ext_idx <- grep("external", colnames(tbl1_df), ignore.case = TRUE)[1]
p_idx <- grep("p.value|p value", colnames(tbl1_df), ignore.case = TRUE)[1]

# 调试输出（可删除）
cat("列索引: Overall=", overall_idx, ", Dev=", dev_idx, ", Ext=", ext_idx, ", P=", p_idx, "\n")

# 创建新行
new_row <- as.data.frame(matrix(NA, nrow = 1, ncol = n_cols))
colnames(new_row) <- colnames(tbl1_df)

new_row[1, 1] <- "中位生存时间 (月), 95%CI"

if (!is.na(overall_idx)) {
  new_row[1, overall_idx] <- overall_med   # 总体中位生存时间
}
if (!is.na(dev_idx)) {
  new_row[1, dev_idx] <- dev_med
}
if (!is.na(ext_idx)) {
  new_row[1, ext_idx] <- ext_med
}
if (!is.na(p_idx)) {
  new_row[1, p_idx] <- p_logrank_formatted
}

tbl1_df <- dplyr::bind_rows(tbl1_df, new_row)

# ------------------- 9. 导出到 Excel -------------------
output_path <- "C:/data/rdata/mlm/Baseline_Table_with_Survival.xlsx"
writexl::write_xlsx(
  list(Table1 = tbl1_df, Normality = normality_df),
  path = output_path
)
message("\n基线表及生存分析结果（含总体中位生存时间）已成功导出至：", output_path)