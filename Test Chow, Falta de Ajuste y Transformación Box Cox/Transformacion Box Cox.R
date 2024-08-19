library(strucchange)
Chow.t <- read.table("D:/Econometria/Chow.txt", header=T)
sctest(Ahorro~Ingreso, data=Chow.t, type = "Chow", point = 9)

library(alr3)
x <- c(1,1,1,2,3,3,4,4,4,4)
y <- c(2.55,2.75,2.57,2.40,4.19,4.70,3.81,4.87,2.93,4.52)
plot(x,y)
m1 <- lm(y~x)
abline(m1)
anova(m1)                # ignora pure error
pureErrorAnova(m1)       # incluye pure error      pure.error.anova


library(RcmdrPlugin.epack)
shapiro.test(residuals(m1))

library(caret)
library(MASS)
library(psych)
library(lmtest)
library(nortest)
data(trees)
shapiro.test(trees$Height)
with(trees,BoxCoxTrans(Height))
shapiro.test(trees$Girth)
with(trees,BoxCoxTrans(Girth))
shapiro.test(trees$Volume)
with(trees,BoxCoxTrans(Volume))
# L2 <- bc2(trees$Volume)
x11()
m1 <- lm(Volume ~ Height + Girth, data = trees)
summary(m1)
shapiro.test(residuals(m1))
bptest(m1)

bc <- boxcox(Volume ~ Height + Girth, data = trees,lambda = seq(0, 0.6, length = 100))
L <- bc$x[which.max(bc$y)]
model1 <- lm(Volume ~ Height + Girth, data = trees)
summary(model1)
shapiro.test(residuals(model1))
ad.test(residuals(model1))
shapiro.test(trees$Volume)
bptest(model1)

tos <- scale(sort(residuals(model1)))
ks.test(tos,pnorm)

trees$Tvol <- (trees$Volume^L-1)/(L*geometric.mean(trees$Volume)^(L-1))
trees$T1vol <- (trees$Volume^L-1)/L
trees$Tpvol <- trees$Volume^L

model2 <- lm(Tvol ~ Height + Girth, data = trees)
summary(model2)
shapiro.test(residuals(model2))
shapiro.test(trees$Tvol)
bptest(model2)

model3 <- lm(T1vol ~ Height + Girth, data = trees)
summary(model3)
shapiro.test(residuals(model3))
bptest(model3)
shapiro.test(trees$T1vol)

model4 <- lm(Tpvol ~ Height + Girth, data = trees)
summary(model4)
shapiro.test(residuals(model4))
bptest(model4)
shapiro.test(trees$Tpvol)

model5 <- lm(log(Volume) ~ Height + log(Girth), data = trees)
summary(model5)
shapiro.test(residuals(model5))
bptest(model5)
shapiro.test(log(trees$Volume))
