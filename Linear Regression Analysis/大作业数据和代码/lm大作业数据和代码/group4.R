Ydata <- read.table("/Users/liuchenghua/R/data/txtGross forestry output1998-2017.txt",header=T,fileEncoding = 'GBK')

City_forestry_station <- read.table("/Users/liuchenghua/R/data/txtCity forestry station number1998-2017.txt",header=T,fileEncoding = 'GBK')

forest_coverage_rate <- read.table("/Users/liuchenghua/R/data/txtforest coverage rate1998-2017.txt",header=T,fileEncoding = 'GBK')

forestry_enterprises<- read.table("/Users/liuchenghua/R/data/txtforestry enterprises1998-2017.txt",header=T,fileEncoding = 'GBK')

forestry_practitioners <- read.table("/Users/liuchenghua/R/data/txtforestry practitioners1998-2017.txt",header=T,fileEncoding = 'GBK')

GDP_per_capita  <- read.table("/Users/liuchenghua/R/data/txtGDP per capita 1998-2017.txt",header=T,fileEncoding = 'GBK')

investment_in_property  <- read.table("/Users/liuchenghua/R/data/txtinvestment in property1998-2017.txt",header=T,fileEncoding = 'GBK')

population_size  <- read.table("/Users/liuchenghua/R/data/txtpopulation size1998-2017.txt",header=T,fileEncoding = 'GBK')

State_investment_in_forestry <- read.table("/Users/liuchenghua/R/data/txtState investment in forestry1998-2017.txt",header=T,fileEncoding = 'GBK')

The_number_of_industrial_enterprises_above_designated_size <- read.table("/Users/liuchenghua/R/data/txtThe number of industrial enterprises above designated size1998-2017.txt",header=T,fileEncoding = 'GBK')


Ydata<-as.matrix(Ydata)
City_forestry_station<-as.matrix(City_forestry_station)
forest_coverage_rate<-as.matrix(forest_coverage_rate)
forestry_enterprises<-as.matrix(forestry_enterprises)
forestry_practitioners<-as.matrix(forestry_practitioners)
GDP_per_capita<-as.matrix(GDP_per_capita)
investment_in_property<-as.matrix(investment_in_property)
The_number_of_industrial_enterprises_above_designated_size<-as.matrix(The_number_of_industrial_enterprises_above_designated_size)
population_size<-as.matrix(population_size)
State_investment_in_forestry<-as.matrix(State_investment_in_forestry)

######################
######################
library(GGally)
library(carData)
library(car)
library(MASS)
########type4######
Y_i=0
CFS_i=0
FCR_i=0
FE_i=0
fer_i=0
GDP_i=0
IP_i=0
PS_i=0
SIF_i=0
NADS_i=0
for (i in c(3,10,12,15,16,17,19,23)){
  Y_i=Y_i+as.matrix(Ydata[i,])
  CFS_i=CFS_i+as.matrix(City_forestry_station[i,])
  FCR_i=FCR_i+as.matrix(forest_coverage_rate[i,])
  FE_i=FE_i+as.matrix(forestry_enterprises[i,])
  fer_i=fer_i+as.matrix(forestry_practitioners[i,])
  GDP_i=GDP_i+as.matrix(GDP_per_capita[i,])
  IP_i=IP_i+as.matrix(investment_in_property[i,])
  PS_i=PS_i+as.matrix(population_size[i,])
  SIF_i=SIF_i+as.matrix(State_investment_in_forestry[i,])
  NADS_i=NADS_i+as.matrix(The_number_of_industrial_enterprises_above_designated_size[i,])
}
Y_i=scale(Y_i)
CFS_i=scale(CFS_i)
FCR_i=scale(FCR_i)
FE_i=scale(FE_i)
fer_i=scale(fer_i)
GDP_i=scale(GDP_i)
IP_i=scale(IP_i)
PS_i=scale(PS_i)
SIF_i=scale(SIF_i)
NADS_i=scale(NADS_i)

A=cbind(Y_i,CFS_i,FCR_i,FE_i,fer_i,GDP_i,IP_i, PS_i,SIF_i,NADS_i)
A<-as.data.frame(A)
ggpairs(A)


fit_i_4<-lm(Y_i~CFS_i+FCR_i+FE_i+fer_i+GDP_i+IP_i+ PS_i+SIF_i+  NADS_i)

summary(fit_i_4)
vif(fit_i_4, digits = 3)

fit_i_4.step <- step(fit_i_4, direction = "backward")
summary(fit_i_4.step)
vif(fit_i_4.step)

C=cbind(GDP_i,IP_i, PS_i)
C<-as.data.frame(C)
economic_pca <- prcomp(C)
screeplot(economic_pca, type='lines')
C<-as.matrix(C)
erot=economic_pca[["rotation"]]
en=C%*%erot
econ_i=en[,1]

end<-lm(Y_i~FE_i+econ_i)
summary(end)
vif(end)
ncvTest(end)
qqnorm(end[["residuals"]])