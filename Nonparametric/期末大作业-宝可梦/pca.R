data <-
  read.table("/Users/liuchenghua/Downloads/hw/非参大作业/Pokemon1.txt",
             header = T)

F = data[, 3:8]
F <- as.matrix(F)
pca <- prcomp(F)
screeplot(pca, type = 'lines')
F <- as.matrix(F)
rot = pca[["rotation"]]
library(ggplot2)
df_pca <- prcomp(F) #计算主成分
df_pcs <- data.frame(df_pca$x, Species = data$Type)
head(df_pcs, 3)  #查看主成分结果
ggplot(df_pcs, aes(
  x = PC1,
  y = PC2,
  z = PC3,
  color = Species
)) + geom_point()
