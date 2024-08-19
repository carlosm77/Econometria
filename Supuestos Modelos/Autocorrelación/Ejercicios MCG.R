#####################################################################
###############  Ejercicios Autocorrelación MCG  ####################
#####################################################################

library(lmtest)
library(car)
library(rms)
library(nlme)

# 1

y<-c(22,26,32,31,40,46,46,50)
x<-c(4,6,10,12,13,16,20,22)
model1 <- lm(y~x)
summary(model1)
# bptest(model1)
dwtest(model1)
durbinWatsonTest(model1,max.lag=6)
W<-matrix(c(1,0.5,0.5^2,0.5^3,0.5^4,0.5^5,0.5^6,0.5^7,0.5,1,0.5,0.5^2,0.5^3,0.5^4,0.5^5,0.5^6,0.5^2,0.5,1,
            0.5,0.5^2,0.5^3,0.5^4,0.5^5,0.5^3,0.5^2,0.5,1,0.5,0.5^2,0.5^3,0.5^4,0.5^4,0.5^3,0.5^2,0.5,1,
            0.5,0.5^2,0.5^3,0.5^5,0.5^4,0.5^3,0.5^2,0.5,1,0.5,0.5^2,0.5^6,0.5^5,0.5^4,0.5^3,0.5^2,0.5,1,0.5,
            0.5^7,0.5^6,0.5^5,0.5^4,0.5^3,0.5^2,0.5,1),ncol=8,nrow=8)
X <-cbind(1,x)
B.MCG <- solve(t(X)%*%solve(W)%*%X)%*%t(X)%*%solve(W)%*%y
B.MCG

model.gls <- gls(y~x,correlation=corAR1(0.5,form = ~ 1, fixed=T))
summary(model.gls)


# 2

t <- 1:10
Periodo <- c(1946,1947,1948,1949,1950,1951,1952,1953,1954,1955)
Ahorro <- c(0.36,0.21,0.08,0.2,0.1,0.12,0.41,0.5,0.43,0.59)
Ingreso <- c(8.8,9.4,10,10.6,11,11.9,12.7,13.5,14.3,15.5)

DF <- data.frame(Periodo,Ahorro,Ingreso)
DF
# a. MCO
model1 <- lm(Ahorro ~ Ingreso,data=DF)
summary(model1)

# b. MCG
Y <- Ahorro
X <- cbind(1,Ingreso)
rho <- 0.2
theta <- -0.5
R1 <- ((theta+rho)*(1+theta*rho))/(1-rho^2)

W <- diag(1+((theta+rho)^2)/(1-rho^2),10)
for (i in 1:nrow(W))
  for (j in 1:ncol(W))
    W[i,j] <- ifelse(i!=j,R1*(rho^abs(i-j)),W[i,j])
W
round(W,3)

P <- solve(W)
B.MCG <- solve(t(X)%*%P%*%X)%*%t(X)%*%P%*%Y
B.MCG
Pred.Ah <- X%*%B.MCG
Res2 <- (Ahorro-Pred.Ah)^2
CME <- sum(Res2)/8
V.MCG <- CME*solve(t(X)%*%P%*%X)
V.MCG

Gls(Ahorro~Ingreso,correlation=corARMA(c(0.2,-0.5),form = ~ 1|t, p=1, q=1, fixed=T))
durbinWatsonTest(model1, max.lag=5)

# weights=varPower(2)
model.gls <- gls(Ahorro~Ingreso,correlation=corARMA(c(0.2,-0.5),form = ~ 1|Periodo , p=1, q=1, fixed=T))
summary(model.gls)
plot(model.gls)
coeftest(model.gls)

# coeftest(model.gls,vcovHAC(model.gls) )

# c. V.MCG
V.MCG <- (summary(model1)$sigma^2)*solve(t(X)%*%solve(W)%*%X)
V.MCG
vcov(model.gls)


# d. Goldfeld-Quandt test
gqtest(model1, order.by = ~ Ingreso, data = DF, fraction = 2, alternative = "two.sided") 
qf(0.95,3,3)

# 3
y <-c(230,140,180,270,300,240,230)
L <- c(30,10,20,40,50,20,30)
K <- c(160,50,100,200,210,190,160)

DF <- data.frame(y,L,K)

# library(nlme)
gls12 <- gls(log(y) ~log(L)+log(K),data=DF,correlation = corARMA(c(0.1, 0, -0.3), form = ~ 1, p = 1, q = 2,fixed=T))
gls12
model.matrix(gls12)
gls12$varBeta
crossprod(gls12$sigma*solve(t(X)%*%P%*%X)) 
plot(ACF(gls12),alpha=0.05)

# library(rms)   
f <- Gls(log(y) ~log(L)+log(K),data=DF, correlation=corARMA(c(0.1, 0, -0.3), form = ~ 1, p = 1, q = 2,fixed=T))
f
AIC(f)
f$var      # bootstrap variances
f$varBeta  # original variances

#Autocorrelacion
# https://www.rpubs.com/Econ0metria/505378
dwtest(model,alternative = "two.sided",iterations = 1000)
durbinWatsonTest(model,max.lag=8)

# b.

theta <- -0.3
rho <- 0.1
g0 <- ((theta^2)+2*(rho^2)*theta+1)/(1-rho^2)
g1 <- rho*g0+theta*rho
g2 <- rho*g1+theta
# g2 <- rho^3*g1+(rho^2)*(theta^2)+rho^3*theta+rho*theta^2
# g3 <- rho^3*g2+theta*rho^2

g3 <- rho*g2
g4 <- rho^2*g2
g5 <- rho^3*g2
g6 <- rho^4*g2

#W1<- matrix(c(g0,g1,g2,g3,g4,g5,g6,g1,g0,g1,g2,g3,g4,g5,g2,g1,g0,g1,g2,g3,g4,g3,g2,g1,g0,g1,g2,g3,g4,
#              g3,g2,g1,g0,g1,g2,g5,g4,g3,g2,g1,g0,g1,g6,g5,g4,g3,g2,g1,g0),ncol=7)


g0 <- (1+rho^2+((theta+rho^2)^2)/(1-rho^2))
#g1 <- rho*(((theta+rho^2)^2)/(1-rho^2)+(theta+rho^2)+1)

W <- diag(g0,7)
for (i in 1:nrow(W))
  for (j in 1:ncol(W))
    W[i,j] <- ifelse(abs(i-j)==1,g1,ifelse(abs(i-j)==2,g2,ifelse(abs(i-j)>=3,(rho^(abs(i-j)-2))*g2,W[i,j])))
# W
round(W,4)

P <- solve(W)
X <- cbind(1,log(L),log(K))
B.MCG <- solve(t(X)%*%P%*%X)%*%t(X)%*%P%*%log(y)
round(B.MCG ,4)

# c.

Pred.Ah <- X%*%B.MCG
Res2 <- (log(y)-Pred.Ah)^2
CME <- sum(Res2)/4
V.MCG <- CME*solve(t(X)%*%solve(W)%*%X)
#V.MCG <- summary(model1)$sigma^2*solve(t(X)%*%solve(W)%*%X)
V.MCG

# d. 
model2 <- lm(residuals(model1)^2 ~log(L)+log(K)+I(log(L)^2)+I(log(K)^2)+log(L)*log(K),data=DF)
summary(model2)
model1 <- lm(log(y) ~log(L)+log(K),data=DF)
summary(model1)
prueba_white<- bptest(log(y) ~log(L)+log(K), ~I(log(L)^2)+I(log(K)^2)+log(L)*log(K),data = DF)
print(prueba_white)
qchisq(0.95,5)


