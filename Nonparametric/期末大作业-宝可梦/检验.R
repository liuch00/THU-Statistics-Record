library(nortest)

#正态性检验
print(lillie.test(Dragon))
print(shapiro.test(Dragon))

print(lillie.test(Electric))
print(shapiro.test(Electric))

print(lillie.test(Fire))
print(shapiro.test(Fire))

print(lillie.test(Grass))
print(shapiro.test(Grass))

print(lillie.test(Steel))
print(shapiro.test(Steel))

print(lillie.test(Water))
print(shapiro.test(Water))

par(mfrow = c(2, 3))
qqnorm(Dragon,
       pch = 20,
       col = "#FA9FB5",
       main = "Dragon")
qqline(Dragon)
qqnorm(Electric,
       pch = 20,
       col = "#FA9FB5",
       main = "Electric")
qqline(Electric)
qqnorm(Fire,
       pch = 20,
       col = "#FA9FB5",
       main = "Fire")
qqline(Fire)
qqnorm(Grass,
       pch = 20,
       col = "#FA9FB5",
       main = "Grass")
qqline(Grass)
qqnorm(Steel,
       pch = 20,
       col = "#FA9FB5",
       main = "Steel")
qqline(Steel)
qqnorm(Water,
       pch = 20,
       col = "#FA9FB5",
       main = "Water")
qqline(Water)

#同方差检验
length(Dragon)
length(Electric)
length(Fire)
length(Grass)
length(Steel)
length(Water)

library(DescTools)
Electric1 = sample(Electric, size = 34)
Fire1 = sample(Fire, size = 34)
Grass1 = sample(Grass, size = 34)
Steel1 = sample(Steel, size = 34)
Water1 = sample(Water, size = 34)

fligner.test(Dragon ~ Electric1)
fligner.test(Dragon ~ Fire1)
fligner.test(Dragon ~ Grass1)
fligner.test(Dragon ~ Steel1)
fligner.test(Dragon ~ Water1)

Electric1 = sample(Electric, size = 44)
Fire1 = sample(Fire, size = 44)
Grass1 = sample(Grass, size = 44)
Steel1 = sample(Steel, size = 44)
Water1 = sample(Water, size = 44)
fligner.test(Electric1 ~ Fire1)
fligner.test(Electric1 ~ Grass1)
fligner.test(Electric1 ~ Steel1)
fligner.test(Electric1 ~ Water1)



Fire1 = sample(Fire, size = 56)
Grass1 = sample(Grass, size = 56)
Steel1 = sample(Steel, size = 44)
Water1 = sample(Water, size = 56)
fligner.test(Fire1 ~ Grass1)
Fire1 = sample(Fire, size = 44)
fligner.test(Fire1 ~ Steel1)
Fire1 = sample(Fire, size = 56)
fligner.test(Fire1 ~ Water1)


Grass1 = sample(Grass, size = 44)
Steel1 = sample(Steel, size = 44)
Water1 = sample(Water, size = 92)
fligner.test(Grass1 ~ Steel1)
Grass1 = sample(Grass, size = 92)
fligner.test(Grass1 ~ Water1)

Steel1 = sample(Steel, size = 44)
Water1 = sample(Water, size = 44)
fligner.test(Steel1 ~ Water1)

#单总体分析
sign.test <- function(x,
                      conf.level = 0.95,
                      conf.int = TRUE) {
  x <- x[order(x)]
  n <- length(x)
  p_sum <- 0
  for (k in 0:n) {
    p_margin <- choose(n, k) / (2 ^ n)
    if (p_sum + p_margin > (1 - conf.level) / 2)
      break
    p_sum = p_sum + p_margin
  }
  return(list(
    estimate = median(x),
    conf.int = c(x[k], x[n + 1 - k]),
    conf.level = 1 - p_sum * 2
  ))
}

print(sign.test(Dragon))
print(sign.test(Electric))
print(sign.test(Fire))
print(sign.test(Grass))
print(sign.test(Steel))
print(sign.test(Water))
library(DescTools)
print(wilcox.test(Dragon, conf.int = TRUE, conf.level = 0.95))
print(wilcox.test(Electric, conf.int = TRUE, conf.level = 0.95))
print(wilcox.test(Fire, conf.int = TRUE, conf.level = 0.95))
print(wilcox.test(Grass, conf.int = TRUE, conf.level = 0.95))
print(wilcox.test(Steel, conf.int = TRUE, conf.level = 0.95))
print(wilcox.test(Water, conf.int = TRUE, conf.level = 0.95))


print(t.test(Dragon, conf.int = TRUE, conf.level = 0.95))
print(t.test(Electric, conf.int = TRUE, conf.level = 0.95))
print(t.test(Fire, conf.int = TRUE, conf.level = 0.95))
print(t.test(Grass, conf.int = TRUE, conf.level = 0.95))
print(t.test(Steel, conf.int = TRUE, conf.level = 0.95))
print(t.test(Water, conf.int = TRUE, conf.level = 0.95))
#双总体分析
iterator <- list(Dragon, Electric, Fire, Grass, Steel, Water)
name <- c('Dragon', 'Electric', 'Fire', 'Grass', 'Steel', 'Water')
for (i in 1:5) {
  for (j in (i + 1):6) {
    print(name[i])
    print(name[j])
    print(wilcox.test(iterator[i][[1]], iterator[j][[1]])$p.value)
  }
}
for (i in 1:5) {
  for (j in (i + 1):6) {
    print(name[i])
    
    print(name[j])
    med <- median(c(iterator[[i]], iterator[[j]]))
    im <- sum(iterator[[i]] < med)
    ip <- sum(iterator[[i]] > med)
    jm <- sum(iterator[[j]] < med)
    jp <- sum(iterator[[j]] > med)
    print(fisher.test(rbind(c(im, ip), c(jm, jp))))
  }
}
