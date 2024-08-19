                        ##############################################
                        ##########     Lack-of-fit test     ##########
                        ##############################################
                        
                        
#library(RcmdrPlugin.epack)
FA <- read.table("D:/CARLOS/Econometría/Ejercicios/Falta de Ajuste.txt", header=T)
Reduced <- lm(Y~X, data=FA)                                       # fit reduced model
summary(Reduced)
Full=lm(Y~0+as.factor(X), data=FA)                                # fit full model
anova(Reduced, Full)                                              # get lack-of-fit test
plot(FA$X,FA$Y)
qf(0.95,11,11)

library(alr3)
pureErrorAnova(Reduced) 
