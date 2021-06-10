data <- read.table("/Users/liuchenghua/Downloads/hw/非参大作业/Pokemon.txt",header=T)
bug<-data[data$Type==Bug]

summary(data$Type)
tapply(data$Total,data$Type,mean)
total=data$Total
type=data$Type
info=cbind(type,total)

Bug =total[1038:1109]  
sd(Bug)
summary(Bug)
Dark=total[990:1037]  
Dragon=total[956:989]  
Electric=total[911:955]  
Fairy =total[874:910] 
Fighting  =total[825:873]
sd(Fighting)
summary(Fighting)
Fire=total[769:824] 
sd(Fire)
summary(Fire)
Flying=total[683:768] 
sd(Flying)
summary(Flying)
Ghost  =total[640:682]
sd(Ghost)
summary(Ghost)
Grass =total[548:639] 
sd(Grass)
summary(Grass)
Ground =total[486:547] 
sd(Ground)
summary(Ground)
Ice  =total[453:485]
sd(Ice)
summary(Ice)
Normal=total[353:452]
sd(Normal)
summary(Normal)
Poison=total[291:352]
sd(Poison)
summary(Poison)
Psychic =total[220:290] 
sd(Psychic)
summary(Psychic)
Rock=total[166:219]
sd(Rock)
summary(Rock)
Steel=total[122:165]
sd(Steel)
summary(Steel)
Water=total[1:121]
sd(Water)
summary(Water)
#绘制箱型图
All1 <- c( Bug , Dark ,Dragon,Electric,  Fairy ,Fighting,Fire,Flying ,  Ghost)
All2 <- c(          Grass,Ground  , Ice  , Normal ,Poison, Psychic ,  Rock,Steel, Water)
All1.label <- c(rep('Bug', length(Bug)), rep('Dark', length(Dark)), rep('Dragon', length(Dragon)),
               rep('Electric', length(Electric)), rep('Fairy', length(Fairy)), rep('Fighting', length(Fighting)),rep('Fire', length(Fire)),
               rep('Flying', length(Flying)), rep('Ghost', length(Ghost))
)
All2.label <- c(rep('Grass', length(Grass)),
               rep('Ground', length(Ground)), rep('Ice', length(Ice)), rep('Normal', length( Normal)),
               rep('Poison', length(Poison)), rep('Psychic', length(Psychic)), 
               rep('Rock', length(Rock)), rep('Steel', length(Steel)), rep('Water', length( Water))
)
boxplot(All1 ~ All1.label, ylab = 'Total')
boxplot(All2 ~ All2.label, ylab = 'Total')
#绘制直方图
par(mfrow=c(2,3))
hist(Bug, xlab = 'Total',prob=TRUE); lines(density(Bug), col="red")
hist(Dark, xlab = 'Total',prob=TRUE);lines(density(Dark), col="red") 
hist(Dragon, xlab = 'Total',prob=TRUE);lines(density(Dragon), col="red")
hist(Electric, xlab = 'Total',prob=TRUE);lines(density(Electric), col="red")
hist(Fairy, xlab = 'Total',prob=TRUE);  lines(density(Fairy), col="red")
hist(Fighting, xlab = 'Total',prob=TRUE);lines(density(Fighting), col="red")

par(mfrow=c(2,3))
hist(Fire, xlab = 'Total',prob=TRUE); lines(density(Fire), col="red")
hist(Flying, xlab = 'Total',prob=TRUE);lines(density(Flying), col="red") 
hist(Ghost, xlab = 'Total',prob=TRUE);lines(density(Ghost), col="red")
hist(Grass, xlab = 'Total',prob=TRUE);lines(density(Grass), col="red")
hist(Ground, xlab = 'Total',prob=TRUE);  lines(density(Ground), col="red")
hist(Ice, xlab = 'Total',prob=TRUE);lines(density(Ice), col="red")

par(mfrow=c(2,3))
hist(Normal, xlab = 'Total',prob=TRUE); lines(density(Normal), col="red")
hist(Poison, xlab = 'Total',prob=TRUE);lines(density(Poison), col="red") 
hist(Psychic, xlab = 'Total',prob=TRUE);lines(density(Psychic), col="red")
hist(Rock, xlab = 'Total',prob=TRUE);lines(density(Rock), col="red")
hist(Steel, xlab = 'Total',prob=TRUE);  lines(density(Steel), col="red")
hist(Water, xlab = 'Total',prob=TRUE);lines(density(Water), col="red")
#绘制核密度估计曲线
brewer.pal(6,"RdPu")
brewer.pal(6,"YlGn")
brewer.pal(6,"Reds")
plot(density(Bug), col="#FEEBE2", xlab = 'total',ylim=c(0,0.005), main = 'Density') 
lines(density(Dark), col="#FCC5C0")
lines(density(Dragon), col="#FA9FB5")
lines(density(Electric), col="#F768A1")
lines(density(Fairy), col="#C51B8A") 
lines(density(Fighting), col="#7A0177") 

lines(density(Fire), col="#FFFFCC")
lines(density(Flying), col="#D9F0A3")
lines(density(Ghost), col="#ADDD8E")
lines(density(Grass), col="#F768A1")
lines(density(Ground), col="#31A354") 
lines(density(Ice), col="#006837") 

lines(density(Normal), col="#FEE5D9")
lines(density(Poison), col="#FCBBA1")
lines(density(Psychic), col="#FC9272")
lines(density(Rock), col="#FB6A4A")
lines(density(Steel), col="#DE2D26") 
lines(density(Water), col="#A50F15") 
