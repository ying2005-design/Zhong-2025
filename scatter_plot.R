# 读取数据文件
data <- read.csv("scatter.csv", stringsAsFactors = FALSE)
# data <- read.delim("node_P_PC_scores.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE)
pca_data <- data.frame(
  Species = data$group,
  PC1 = data$PC1,
  PC2 = data$PC2
)

library(ggplot2)

# 绘制散点图
ggplot(pca_data, aes(x = PC1, y = PC2, color = Species, fill = Species)) +
  # 添加置信椭圆
  stat_ellipse(type = "norm", level = 0.9, geom = "polygon", alpha = 0.1,
               linetype = "dashed", size = 0.5, color = "black") +

  # 加粗轴线
  geom_hline(yintercept = 0, color = "black", size = 0.8, alpha = 0.4) +
  geom_vline(xintercept = 0, color = "black", size = 0.8, alpha = 0.4) +

  # 添加散点，使用不同形状并添加描边
  geom_point(aes(shape = Species), size = 3, alpha = 0.7,
             color = "black", stroke = 0.8) +

  # 设置颜色和形状
  scale_fill_manual(values = c("nrtran" = "#F5AE6B", "dent" = "#4387B5", "gigas" = "#7C7CBA", "noda" = "#B55489")) +
  scale_shape_manual(values = c("nrtran" = 21, "dent" = 22, "gigas" = 23, "noda" = 24)) +  # 21=圆形，24=三角形

  # 设置标题和坐标轴标签
  labs(
    x = "PC1: 93.74%",
    y = "PC2: 3.361%"
  ) +
  # 使用经典主题并自定义
  theme_minimal() +

  # 自定义主题元素
  theme(
    text = element_text(family = "Times New Roman"),
    plot.title = element_text(hjust = 0.5, face = "bold"),

    # axis
    axis.title.x = element_text(size=15),
    axis.text.x=element_text(size=14),
    axis.title.y = element_text(size=15),
    axis.text.y=element_text(size=14),

    # legend
    legend.position = c(0,1),
    legend.justification=c(0,1),
    legend.text=element_text(size=16),
    legend.key.size = unit(1.5, "lines"),
    legend.title = element_blank(),

    panel.border = element_rect(color = "black", fill = NA, size = 1.5)  # 添加图表边框
  )

# 保存图形
ggsave("mm_pca.svg",
       width = 12, height = 9, bg = "white") # 4:3