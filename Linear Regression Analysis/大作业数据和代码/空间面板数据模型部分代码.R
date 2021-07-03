N = 31 #省份总数
T = 20 #时间段总数(1998-2017)
library(splm)
library(spatialreg)
setwd("/Users/sj-zhang/Desktop/线性回归大作业/Project选题/附件2：林业总产值数据与参考文献")
Ydata=read.csv("./响应变量Y/Gross forestry output1998-2017.csv", header = T, col.names = 1998:2017)
X1data=read.csv("./整理后的解释变量X（仅供参考）/City forestry station number1998-2017.csv",header = T, col.names = 1998:2017)
X2data=read.csv("./整理后的解释变量X（仅供参考）/forest coverage rate1998-2017.csv",header = T, col.names = 1998:2017)
X3data=read.csv("./整理后的解释变量X（仅供参考）/forestry enterprises1998-2017.csv",header = T, col.names = 1998:2017)
X4data=read.csv("./整理后的解释变量X（仅供参考）/forestry practitioners1998-2017.csv",header = T, col.names = 1998:2017)
X4data2=read.csv("./整理后的解释变量X（仅供参考）/forestry practitioners1998-2017的副本.csv",header = T, col.names = 1998:2017)
X5data=read.csv("./整理后的解释变量X（仅供参考）/GDP per capita 1998-2017.csv",header = T, col.names = 1998:2017)
X6data=read.csv("./整理后的解释变量X（仅供参考）/investment in property1998-2017.csv",header = T, col.names = 1998:2017)
X7data=read.csv("./整理后的解释变量X（仅供参考）/population size1998-2017.csv",header = T, col.names = 1998:2017)
X8data=read.csv("./整理后的解释变量X（仅供参考）/State investment in forestry1998-2017.csv",header = T, col.names = 1998:2017)
X9data=read.csv("./整理后的解释变量X（仅供参考）/The number of industrial enterprises above designated size1998-2017.csv",header = T, col.names = 1998:2017)
distances=read.csv("全国31省区（除港澳台）的地理距离.csv",header=F)
Provinces = c("北京","天津","河北","山西","内蒙古","辽宁","吉林","黑龙江","上海","江苏","浙江","安徽","福建","江西","山东","河南","湖北","湖南","广东","广西","海南","重庆","四川","贵州","云南","西藏","陕西","甘肃","青海","宁夏","新疆")
Provinces1 = rep(Provinces, each = 20)
Province = factor(Provinces1, level = Provinces)
Year = rep(1998:2017, 31)
Y = {}
X1 = {}
X2 = {}
X3 = {}
X4 = {}
X5 = {}
X6 = {}
X7 = {}
X8 = {}
X9 = {}
for(i in 1:31)
{
  Y = cbind(Y, as.numeric(Ydata[i,1:20]))
  X1 = cbind(X1, as.numeric(X1data[i,1:20]))
  X2 = cbind(X2, as.numeric(X2data[i,1:20]))
  X3 = cbind(X3, as.numeric(X3data[i,1:20]))
  X4 = cbind(X4, as.numeric(X4data[i,1:20]))
  X5 = cbind(X5, as.numeric(X5data[i,1:20]))
  X6 = cbind(X6, as.numeric(X6data[i,1:20]))
  X7 = cbind(X7, as.numeric(X7data[i,1:20]))
  X8 = cbind(X8, as.numeric(X8data[i,1:20]))
  X9 = cbind(X9, as.numeric(X9data[i,1:20]))
}
Y = as.vector(Y)
X1 = as.vector(X1)
X2 = as.vector(X2)
X3 = as.vector(X3)
X4 = as.vector(X4)
X5 = as.vector(X5)
X6 = as.vector(X6)
X7 = as.vector(X7)
X8 = as.vector(X8)
X9 = as.vector(X9)
dataset = data.frame(Province,Year,Y,X1,X2,X3,X4,X5,X6,X7,X8,X9)
log_dataset = dataset
for(i in 3:12)
{
  log_dataset[,i] = log(dataset[,i] + 2)
}
log_dataset
Wn = matrix(NA,31,31)
Wn = as.matrix(distances)
Wn = 1000/Wn
Wn
for(i in 1:31)
{
  Wn[i,i] = 0
  Wn[i,] = Wn[i,]/sum(Wn[i,])
}
Wn
rownames(Wn) = Provinces
colnames(Wn) = Provinces
Wn0 = mat2listw(Wn)
model1 = spml(Y~X2+X5+X7+X9,data=log_dataset,index=NULL,listw=Wn0,model="random",lag=T,spatial.error="b")
summary(model1)
model2 = spml(Y~X2+X5+X7+X9,data=log_dataset,index=NULL,listw=Wn0,model="random",effect="individual",lag=T,spatial.error="b")
summary(model2)
model3 = spml(Y~X2+X5+X7+X9,data=log_dataset,index=NULL,listw=Wn0,model="within",effect="individual",lag=T,spatial.error="b")
summary(model3)

