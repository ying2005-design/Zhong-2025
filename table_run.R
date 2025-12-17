# 读取数据
df <- read.csv("CS_removed_RAV_corrected_sensu.csv", stringsAsFactors = FALSE)

# 获取物种分组（排除第一列NAME和第二列SP）
species_groups <- unique(df$SP)
measurement_names <- names(df)[-c(1, 2)]  # 排除NAME和SP列

# 计算每个物种的标本数量
species_counts <- table(df$SP)

# 创建空的结果数据框
result_df <- data.frame(
  Measurement = measurement_names,
  stringsAsFactors = FALSE
)

# 为每个物种分组添加列（包含标本数量）
for(species in species_groups) {
  # 筛选当前物种的数据
  species_data <- df[df$SP == species, -c(1, 2)]  # 排除NAME和SP列

  # 对每个测量特征计算统计量
  stats_vector <- sapply(measurement_names, function(feature) {
    values <- species_data[[feature]]
    if(length(values) > 0 && !all(is.na(values))) {
      mean_val <- mean(values, na.rm = TRUE)
      sd_val <- sd(values, na.rm = TRUE)
      min_val <- min(values, na.rm = TRUE)
      max_val <- max(values, na.rm = TRUE)

      # 使用4位小数固定精度
      sprintf("%.3f ± %.3f [%.3f,%.3f]", mean_val, sd_val, min_val, max_val)
    } else {
      "NA"
    }
  })

  # 将结果添加到数据框，列名包含标本数量
  col_name <- paste0(species, " (n=", species_counts[species], ")")
  result_df[[col_name]] <- stats_vector
}

# 查看结果
print(result_df)

# 保存结果到CSV文件
write.csv(result_df, "measurement_statistics.csv", row.names = FALSE)

# 可选：在控制台中以更易读的格式显示
cat("\n=== 测量特征统计摘要 (包含标本数量) ===\n")
for(i in 1:nrow(result_df)) {
  cat("\n", result_df$Measurement[i], ":\n")
  for(j in 2:ncol(result_df)) {
    cat("  ", names(result_df)[j], ": ", result_df[i, j], "\n")
  }
}