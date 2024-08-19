library(strucchange)
Chow.t <- read.table("D:/CARLOS/Econometria/Ejercicios/Chow.txt", header=T)
sctest(Ahorro~Ingreso, data=Chow.t, type = "Chow", point = 9)

model1 <- lm(Ahorro~Ingreso, data=Chow.t)
summary(model1)

library(lmtest)
dwtest(model1)
ACF <- acf(residuals(model1),lag.max=15)
PACF <- pacf(residuals(model1))

# MCG
AI <- Chow.t[1:4,-1]
rho <- 0.3
rho2 <- rho^2
rho3 <- rho^3
W <- matrix(c(1,rho,rho2,rho3,rho,1,rho,rho2,rho2,rho,1,rho,rho3,rho2,rho,1),ncol=4)
W
X <- cbind(1,AI$Ingreso)
Y <- AI$Ahorro
B.MCG <- solve(t(X)%*%solve(W)%*%X)%*%t(X)%*%solve(W)%*%Y
B.MCG

Omega <- diag(4)

for (i in 1:4) 
    for (j in 1:4)
Omega[i,j] <- ifelse(i!=j,rho^abs(i-j),Omega[i,j])
Omega


model.mcp <- lm(Ahorro~Ingreso, data=AI,weights = c(1,1,1,1))
summary(model.mcp)

model.ols <- lm(Ahorro~Ingreso, data=AI)
summary(model.ols)

install.packages("alr3", repos="http://R-Forge.R-project.org")
library(alr3)
x <- c(1,1,1,2,3,3,4,4,4,4)
y <- c(2.55,2.75,2.57,2.40,4.19,4.70,3.81,4.87,2.93,4.52)
plot(x,y)
m1 <- lm(y~x)
abline(m1)
anova(m1)                # ignora pure error
pureErrorAnova(m1)       # incluye pure error      pure.error.anova

library(olsrr)
ols_pure_error_anova(m1)

library(readxl)
Falta_de_Ajuste <- read_excel("D:/CARLOS/Drive Econometria/Test Chow, Falta de Ajuste y Transformación Box Cox/Falta de Ajuste.xlsx")
View(Falta_de_Ajuste)

Model1 <- lm(Y~X,data=Falta_de_Ajuste)
ols_pure_error_anova(Model1)

library(RcmdrPlugin.epack)

shapiro.test(residuals(m1))

#Transformacion Box-Cox
library(MASS)
library(psych)
library(caret)
library(nortest)

data(trees)
L2 <- BoxCoxTrans(trees$Volume)$lambda
BoxCoxTrans(trees$Height)
BoxCoxTrans(trees$Girth)

model0 <- lm(Volume ~ Height + Girth, data = trees)
summary(model0)
shapiro.test(residuals(model0))

bc <- boxcox(Volume ~ Height + Girth, data = trees,lambda = seq(-1, 2, length = 20))
L <- bc$x[which.max(bc$y)]
trees$Tvol <- (trees$Volume^L-1)/(L*geometric.mean(trees$Volume)^(L-1))
trees$T1vol <- (trees$Volume^L-1)/L
trees$Tpvol <- trees$Volume^L

model1 <- lm(Tpvol ~ I(Height^2) + log(Girth), data = trees)
summary(model1)
shapiro.test(residuals(model1))
ad.test(residuals(model1))
shapiro.test(trees$Volume)

tos <- scale(sort(residuals(model1)))
ks.test(tos,pnorm)


model2 <- lm(Tvol ~ I(Height^2) + log(Girth), data = trees)
summary(model2)
shapiro.test(residuals(model2))
shapiro.test(trees$Tvol)

model3 <- lm(T1vol ~ I(Height^2) + log(Girth), data = trees)
summary(model3)
shapiro.test(residuals(model3))
shapiro.test(trees$T1vol)

model4 <- lm(Tpvol ~ I(Height^2) + log(Girth), data = trees)
summary(model4)
shapiro.test(residuals(model4))
shapiro.test(trees$Tpvol)

model5 <- lm(log(Volume) ~ log(Height) + log(Girth), data = trees)
summary(model5)
shapiro.test(residuals(model5))
bptest(model5)
shapiro.test(log(trees$Volume))
