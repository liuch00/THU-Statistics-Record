
sf <- read.table("/Users/liuchenghua/R/data/shengfen.txt",header=T,fileEncoding = 'GBK')
sf<-scale(sf)
kmeans(sf,4, nstart = 20)
