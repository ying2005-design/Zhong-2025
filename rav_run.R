# 加载必要的包
library(dplyr)
library(tidyr)
library(purrr)

# 读取数据
data <- read.csv("CS_removed_raw.csv", stringsAsFactors = FALSE)
colnames(data)[1] <- "NAME"
colnames(data)[2] <- "SP"
colnames(data)[3] <- "CS"

# 查看数据结构
cat("数据维度:", dim(data), "\n")
cat("物种分组:", unique(data$SP), "\n")
cat("特征列:", colnames(data)[4:ncol(data)], "\n")

# 设置标准头大小 (CSS) - 使用所有个体的CS平均值
CSS <- mean(data$CS)
cat("标准头大小 CSS =", round(CSS, 4), "mm\n")

# 进行RAV分析的主函数
perform_rav_analysis <- function(data, css_value) {

  # 获取所有需要校正的特征列名（排除NAME, SP, CS）
  feature_columns <- colnames(data)[4:ncol(data)]

  # 存储结果的列表
  rav_results <- list()
  correction_functions <- list()

  # 对每个特征进行RAV校正
  for(feature in feature_columns) {

    # 为每个物种建立线性回归模型
    species_models <- list()
    species_list <- unique(data$SP)

    for(species in species_list) {
      species_data <- data[data$SP == species, ]
      model <- lm(as.formula(paste(feature, "~ CS")), data = species_data)
      species_models[[species]] <- list(
        a = coef(model)[2],  # 斜率
        b = coef(model)[1]   # 截距
      )
    }

    # 计算全局参数（所有物种的平均）
    a_global <- mean(map_dbl(species_models, ~ .x$a))
    b_global <- mean(map_dbl(species_models, ~ .x$b))

    # 构建全局描述符函数
    descriptor_function <- function(cs) {
      a_global * cs + b_global
    }

    # 计算校准常数
    c_cal <- descriptor_function(css_value)

    # 对每个个体进行RAV校正
    rav_values <- numeric(nrow(data))
    for(i in 1:nrow(data)) {
      original_ratio <- data[i, feature]
      cs_i <- data[i, "CS"]
      d_global_i <- descriptor_function(cs_i)
      rav_values[i] <- (original_ratio / d_global_i) * c_cal
    }

    # 存储结果
    rav_results[[feature]] <- rav_values

    # 存储校正函数信息
    correction_functions[[feature]] <- list(
      feature = feature,
      a_global = a_global,
      b_global = b_global,
      c_cal = c_cal,
      css = css_value
    )
  }

  return(list(
    rav_data = rav_results,
    functions = correction_functions
  ))
}

# 执行RAV分析
results <- perform_rav_analysis(data, CSS)

# 创建RAV校正后的数据框
rav_corrected_data <- data.frame(
  NAME = data$NAME,
  SP = data$SP,
  CS = data$CS
)

# 添加RAV校正后的特征值
for(feature in names(results$rav_data)) {
  rav_corrected_data[[paste0(feature, round(CSS, 4))]] <- results$rav_data[[feature]]
}

# 保存RAV校正后的数据到CSV文件
write.csv(rav_corrected_data, "RAV_corrected_data.csv", row.names = FALSE)
cat("RAV校正后的数据已保存到: RAV_corrected_data.csv\n")

# 生成校正函数文本
function_text <- paste0(
  "RAV was calculated for the assumption of all individuals having the same head size of CS = ",
  round(CSS, 4), " mm:\n\n"
)

for(func_info in results$functions) {
  feature <- func_info$feature
  a <- round(func_info$a_global, 4)
  b <- round(func_info$b_global, 4)
  c_cal <- round(func_info$c_cal, 4)
  css <- round(func_info$css, 4)

  # 确定符号
  sign <- ifelse(a >= 0, "+", "-")
  abs_a <- abs(a)

  function_text <- paste0(
    function_text,
    feature, css, " = ", feature, " /(", sign, " ", abs_a, "*CS + ", b, ") * ", c_cal, "\n"
  )
}

# 保存校正函数到文本文件
writeLines(function_text, "RAV_correction_functions.txt")
cat("RAV校正函数已保存到: RAV_correction_functions.txt\n")

# 显示部分结果
cat("\n前5个样本的RAV校正结果:\n")
print(head(rav_corrected_data, 5))

cat("\n生成的校正函数:\n")
cat(function_text)

# 绘制原始数据与RAV校正后数据的比较图
if(require(ggplot2)) {
  # 选择一个特征进行可视化
  example_feature <- colnames(data)[4]  # 使用第一个特征

  # 准备数据
  plot_data <- data.frame(
    NAME = data$NAME,
    SP = data$SP,
    CS = data$CS,
    Original = data[[example_feature]],
    RAV_Corrected = rav_corrected_data[[paste0(example_feature, round(CSS, 4))]]
  )

  # 创建散点图
  p1 <- ggplot(plot_data, aes(x = CS, y = Original, color = SP)) +
    geom_point() +
    labs(title = paste("Raw:", example_feature),
         x = "CS (mm)", y = example_feature) +
    theme_bw()

  p2 <- ggplot(plot_data, aes(x = CS, y = RAV_Corrected, color = SP)) +
    geom_point() +
    labs(title = paste("RAV:", example_feature),
         x = "CS (mm)", y = paste0(example_feature, round(CSS, 4))) +
    theme_bw()

  # 保存图形
  ggsave("RAV_comparison_plot.png",
         gridExtra::grid.arrange(p1, p2, ncol = 2),
         width = 10, height = 5)
  cat("\n比较图已保存到: RAV_comparison_plot.png\n")
}

cat("\nRAV分析完成!\n")