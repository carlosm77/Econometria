library(strucchange)
Chow.t <- read.table("D:/CARLOS/Drive Economeria/Test Chow, Falta de Ajuste y Transformación Box Cox/Chow.txt", header=T)
sctest(Ahorro~Ingreso, data=Chow.t, type = "Chow", point = 9)

library(lmtest)
model1 <- lm(Ahorro~Ingreso, data=Chow.t[1:10,])
dwtest(Ahorro~Ingreso, data=Chow.t,alternative="two.side" )
Box.test(residuals(model1), lag = 4, type = "Ljung")
acf(residuals(model1))
pacf(residuals(model1))
bgtest(model1)
bgtest(model1,order=2)

library(alr3)
x <- c(1,1,1,2,3,3,4,4,4,4)
y <- c(2.55,2.75,2.57,2.40,4.19,4.70,3.81,4.87,2.93,4.52)
plot(x,y)
m1 <- lm(y~x)
abline(m1)
anova(m1)                # ignora pure error
pureErrorAnova(m1)       # incluye pure error      pure.error.anova