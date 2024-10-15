library(openxlsx)
library(pheatmap)
library(RColorBrewer)

# 读取数据
df <- read.xlsx("D:/nuclear-cytoplasmic interaction/GWAS/data.xlsx", sheet = "Sheet10")

# 查看数据结构
str(df)

# 将基因型数据转换为数值
convert_genotype_to_numeric <- function(genotype) {
  if (genotype == "0/0") {
    return(0)
  } else if (genotype == "0/1" || genotype == "1/0") {
    return(1)
  } else if (genotype == "1/1") {
    return(2)
  } else {
    return(NA)
  }
}

# 应用转换函数到数据框
df_numeric <- as.data.frame(lapply(df, function(column) {
  sapply(column, convert_genotype_to_numeric)
}))

# 设置行名为样本ID（假设样本ID在第一列）
row.names(df_numeric) <- df[,1]
df_numeric <- df_numeric[,-1]

# 将缺失值标记为 -1
df_numeric[is.na(df_numeric)] <- -1

# 绘制热图，调大字号
# 绘制热图，调大字号并增加聚类树的可见性
pheatmap(df_numeric, 
         cluster_rows = F, 
         cluster_cols = T,
         color = colorRampPalette(brewer.pal(n = 9, name = "Blues"))(100),
         na_col = "white", # 确保缺失值显示为白色
         border_color = NA, # 取消单元格边框
         show_colnames = FALSE, # 去掉横坐标标签
         fontsize = 12, # 调整热图中文字的字号大小
         fontsize_row = 30, # 调整行标签的字号大小
         fontsize_col = 20, # 调整列标签的字号大小
         treeheight_col = 50, # 增加列聚类树的高度
         lwd = 4) # 增加线条粗细