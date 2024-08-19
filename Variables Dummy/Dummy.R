pureErrorAnova <- function(mod){UseMethod("pureErrorAnova")}
pureErrorAnova.lm <- function(mod) {
  if (is.null(mod$model)) mod <- update(mod, model=TRUE)
  p <- dim(mod$model)[2] -1
  mod$model$Lack.of.Fit <-
    factor(randomLinComb(model.matrix(mod), 101319853))
  aov1 <- anova(mod)
  #set.seed(save.seed) # restore random number seed
  if (length(levels(mod$model$Lack.of.Fit)) == length(mod$model$Lack.of.Fit))
    aov1 else {
      aov2 <- anova(lm(mod$model[ , 1]~mod$model$Lack.of.Fit, weights=weights(mod)))
      rrow <- dim(aov1)[1]
      aov2[1, 1] <- aov1[rrow, 1]-aov2[2, 1]
      aov2[1, 2] <- aov1[rrow, 2]-aov2[2, 2]
      aov2[1, 3] <- aov2[1, 2]/aov2[1, 1]
      aov1[1:(rrow-1), 4] <- aov1[1:(rrow-1), 3]/aov2[2, 3]
      aov2[1, 4] <- aov2[1, 3]/aov2[2, 3]
      row.names(aov2) <- c(" Lack of fit", " Pure Error")
      aov <- rbind(aov1, aov2)
      aov[ , 5] <- pf(aov[ , 4], aov[ , 1], aov2[2, 1], lower.tail=FALSE)
      aov
    }}



library(RcmdrPlugin.epack)
library(readxl)
Personas <- read_excel("D:/CARLOS/Drive Econometria/Ejercicios/PEst 2022-II 65.xlsx")

Peso <- c(58,84,46,54,48,88,75,54,63,55)
Estatura <- c(1.58,1.73,1.6,1.78,1.58,1.79,1.68,1.54,1.78,1.68)
Personas <- data.frame(Obs=1:10,Peso,Estatura)
Personas$Genero <- c(1,2,1,2,1,2,2,1,2,1)

Personas$Mujer <- ifelse(Personas$Genero==1,1,0)
Personas$Hombre <- ifelse(Personas$Genero==2,1,0)
model1 <- lm(Peso ~ Hombre, data=Personas)
Anov <- anova(model1)
summary(model1)

mean(Personas[Personas$Genero==1,]$Peso)
mean(Personas[Personas$Genero==2,]$Peso)

Y <- as.matrix(Personas$Peso,ncol=1)
X <- cbind(1,Personas$Hombre)
Betas <- solve(t(X)%*%X,t(X)%*%Y)
Inv.X <- solve(t(X)%*%X)
V.Betas <- Anov["Mean Sq"][2,]*Inv.X
V.Betas <- vcov(model1)
des.est.Bo <- sqrt(diag(V.Betas))[1]
des.est.B1 <- sqrt(diag(V.Betas))[2]

confint(model1,level=0.95)

Ft <- qf(0.99,1,8) 
Tt <- qt(0.975,8)
c(Betas[1]-Tt*des.est.Bo,Betas[1]+Tt*des.est.Bo)
c(Betas[2]-Tt*des.est.B1,Betas[2]+Tt*des.est.B1)

Personas$Lpeso <- log(Personas$Peso)

model2 <- lm(Peso ~ Estatura, data=Personas)
Anov1 <- anova(model2)
summary(model2)
X <- cbind(1,Personas$Estatura)
Inv.X <- solve(t(X)%*%X)
Xo <- c(1,176)
CME <- Anov1["Mean Sq"][2,]

Yp <- t((summary(model2))$coefficients[,1])%*%(Xo)
VYp <- CME*(as.matrix(t(Xo))%*%Inv.X%*%t(as.matrix(t(Xo)))+1)

Tt <- qt(0.99,8)

predict(model1, newdata=data.frame(Hombre=0), se=T, interval='confidence', level=0.98)



Xo1 <- data.frame(c(1,0))
colnames(Xo1) <- "Mujer"
predict(model1, newdata=Xo1, se=T, interval='confidence', level=0.98)

#------------------
# Falta de Ajuste
#------------------

Obs <- 1:24
Y <- c(2.3,1.8,2.8,1.5,2.2,3.8,1.8,3.7,1.7,2.8,2.8,2.2,5.4,3.2,1.9,1.8,3.5,2.8,2.1,3.4,3.2,3.0,3.0,5.9)
X <- c(1.3,1.3,2.0,2.0,2.7,3.3,3.3,3.7,3.7,4.0,4.0,4.0,4.7,4.7,4.7,5.0,5.3,5.3,5.3,5.7,6.0,6.0,6.3,6.7)

FA <- data.frame(Obs,Y,X)
plot(X,Y)

Reduced <- lm(Y~X, data=FA)                                       # fit reduced model
summary(Reduced)
Full=lm(Y~0+as.factor(X), data=FA)                                # fit full model
anova(Reduced, Full)                                              # get lack-of-fit test
plot(FA$X,FA$Y)
qf(0.95,2,14)
qf(0.95,11,11)

library(alr3)
pureErrorAnova(Reduced) 

#------------------
# Test de Chow
#------------------

Chow <- read_excel("D:/CARLOS/Econometr?a/Ejercicios/Chow.xlsx")

library(gap)

y1<-Chow[1:9,2]
y2<-Chow[10:18,2]
x1<-Chow[1:9,3]
x2<-Chow[10:18,3]
chow.test.r<-chow.test(y1,x1,y2,x2)

library(strucchange)
sctest(Ahorro~Ingreso, data=Chow, type = "Chow", point = 9)


model1 <- lm(Ahorro~Ingreso, data=Chow)
model2 <- lm(Ahorro~Ingreso, data=Chow[1:9,])
model3 <- lm(Ahorro~Ingreso, data=Chow[10:18,])
Anov1 <- anova(model1)
SCE1 <- Anov1["Sum Sq"][2,]

Anov2 <- anova(model2)
SCE2 <- Anov2["Sum Sq"][2,]

Anov3 <- anova(model3)
SCE3 <- Anov3["Sum Sq"][2,]

S4 <- SCE2+SCE3
S5 <- SCE1 -S4

Fc <- (S5/2)/(S4/(9+9-2*2))
Ft <- qf(0.95,2,14)

library(RcmdrPlugin.epack)
library(RODBC, pos=25)
Chow <- sqlQuery(channel = 1, select*from[Hoja1])
Dummy <- sqlQuery(channel = 2, select * from [Hoja1])


#------------------
# Variables Dummy
#------------------
#install.packages("readxl")
library(readxl)
Dummy <- read_excel("D:/CARLOS/Econometria/Ejercicios/Dummy12.03.13.xlsx")
View(Dummy)
Dummy <- as.data.frame(Dummy)

Dummy$A <- ifelse(Dummy$ZONA=="A",1,0)
Dummy$B <- ifelse(Dummy$ZONA=="B",1,0)
Dummy$Z <- ifelse(Dummy$ZONA=="A",Dummy$X,0)

# Asignar colores segun zona
Dummy$col_zona <- ifelse(Dummy$ZONA == "A", "red","blue")

# Plotear los puntos con informacion de zona en color
plot(Dummy$X,Dummy$Y, col = Dummy$col_zona, asp = 1)
legend(locator(1), col = c("red", "blue"), legend = c("A", "B"), bty = 'n', pch = 1, title = "zona")
# Plotear los puntos con ggplot
library(ggplot2)
ggplot(data = Dummy) +
  geom_point(mapping = aes(x = X, y = Y, color = col_zona))

ggplot(data = Dummy) +
  geom_point(mapping = aes(x = X, y = Y, size = X, color = col_zona))


model1 <- lm(Y~X, data=Dummy)
model2 <- lm(Y~X, data=Dummy[Dummy$ZONA=="A",])
model3 <- lm(Y~X, data=Dummy[Dummy$ZONA=="B",])
model4 <- lm(Y~X+A, data=Dummy)
model5 <- lm(Y~X+A+Z, data=Dummy)
model6 <- lm(Y~A, data=Dummy)
model7 <- lm(Y~B, data=Dummy)
model8 <- lm(Y~as.factor(ZONA), data=Dummy)

summary(model1)
summary(model2)
summary(model3)
summary(model4)
summary(model5)
summary(model6)
summary(model7)
summary(model8)

model9 <- lm(Y~A+B+0, data=Dummy)
summary(model9)

mean(Dummy[Dummy$ZONA=="A",]$Y)
mean(Dummy[Dummy$ZONA=="B",]$Y)

#save.image("D:/compartido/Econometria/Dummy.Rdata")
load("D:/compartido/Econometria/Dummy.Rdata")


library(nortest)
shapiro.test(Dummy$residuals)
sf.test(Dummy$residuals)
Res.order <- Dummy$residuals[order(Dummy$residuals)]
ks.test(Res.order)

shapiro.test(residuals)
shapiro.test(Y)


RegModel.1 <- lm(VALOR.HA~AGUA+AREA+CA2+CA3+CA4+ORDPUBL+UBI2+VIA2+VIA3+VP, 
                 data=Rural)
summary(RegModel.1)

Rural$residuales <- residuals(RegModel.1)
shapiro.test(Rural$residuales)
shapiro.test(Rural$VALOR.HA)
shapiro.test(Rural$AREA)

with(Rural, Hist(residuales, scale="frequency", breaks=15, col="darkgray"))
with(Rural, Hist(VALOR.HA, scale="frequency", breaks=15, col="darkgray"))
with(Rural, Hist(AREA, scale="frequency", breaks=15, col="darkgray"))

densityPlot( ~ AREA, data=Rural, bw="SJ", adjust=1, 
             kernel="gaussian")
densityPlot( ~ VALOR.HA, data=Rural, bw="SJ", adjust=1, 
             kernel="gaussian")

bc2(Rural$AREA)
bc.model1 <- boxcox(VALOR.HA~AGUA+AREA+CA2+CA3+CA4+ORDPUBL+UBI2+VIA2+VIA3+VP, 
       data=Rural,   lambda = seq(-3.0, 3.0, length = 50))

bc.model1$x[which.max(bc.model1$y)]

Rural$LAREA <- log(Rural$AREA)
Rural$LVALOR.HA <- log(Rural$VALOR.HA)
Rural$TBCVALOR.HA <- Rural$VALOR.HA^(bc.model1$x[which.max(bc.model1$y)])

shapiro.test(Rural$LVALOR.HA)
shapiro.test(Rural$TBCVALOR.HA)

library(het.test)
whites.htest(RegModel.1)


RegModel.2 <- lm(LVALOR.HA~AGUA+AREA+CA2+CA3+CA4+ORDPUBL+UBI2+VIA2+VIA3+VP, 
                 data=Rural)
summary(RegModel.2)

RegModel.3 <- lm(LVALOR.HA~LAREA+VP, data=Rural)
summary(RegModel.3)
shapiro.test(residuals(RegModel.3))

RegModel.4 <- lm(TBCVALOR.HA~LAREA+VP, data=Rural)
summary(RegModel.4)
shapiro.test(residuals(RegModel.4))


bc2(Rural$VALOR.HA)
bc2(Rural$AREA)

densityPlot( ~ TBCVALOR.HA, data=Rural, bw="SJ", adjust=1, 
             kernel="gaussian")
densityPlot( ~ residuals.RegModel.3, data=Rural, bw="SJ", adjust=1, 
             kernel="gaussian")





