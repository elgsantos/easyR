setwd("~/EasyR/minicurso")
getwd()

env <- read.csv("Doubs.csv",header=T,sep=",",dec=".")
names(env)

#nitrato, altitude, declividade [pen], vazao [deb]
jpeg("Variaveis_aula4.jpg", width=200, height = 200, bg="white", res=300, unit="mm")
par(mfrow=c(2,2),mar=c(4.5,4.5,1,1))
plot(env$das, env$nit,type="l",xlab="Distancia da fonte (km)",
     ylab="Nitrato (mg/L)",col="red",lwd=2)
plot(env$das, env$alt,type="l",xlab="Distancia da fonte (km)",
     ylab="Altitude (m)",col="darkgreen",lwd=2)
plot(env$das, env$pen,type="l",xlab="Distancia da fonte (km)",
     ylab="Declividade (%)",col="darkorange",lwd=2)
plot(env$das, env$deb,type="l",xlab="Distancia da fonte (km)",
     ylab=expression(paste("Vazão média (m"^3,"s"^-1,")")),col="blue",lwd=2)
dev.off()

#relacoes
jpeg("Relacao_var.jpg", width=200, height = 200, bg="white", res=300, unit="mm")
par(mfrow=c(2,2),mar=c(4.5,4.5,1,1))
plot(env$alt,env$nit,xlab="Altitude (m)",ylab="Nitrato (mg/L)",pch=19,col="red")
plot(env$deb, env$nit,xlab=expression(paste("Vazão média (m"^3,"s"^-1,")")),
     ylab="Nitrato (mg/L)",pch=21, bg="white", col="red")
plot(log(env$alt),log(env$nit),xlab="log(Altitude)",ylab="log(Nitrato)",pch=19,col="red")
plot(log(env$deb), log(env$nit),xlab=expression(paste("log(Vazão)")),
     ylab="log(Nitrato)",pch=21, bg="white", col="red")
dev.off()

#calculando a mão
dados <- data.frame(XY=(env$alt)*(env$nit),
                    X=env$alt,
                    Y=env$nit,
                    X2=(env$alt)^2)
head(dados)
b <- sum(dados$XY) - (sum(dados$X) *sum(dados$Y))/nrow(env)
b
b <- b / (sum(dados$X2) - (sum(dados$X)^2)/nrow(env))
b
a<- mean(dados$Y) - b*mean(dados$X)
a
#linear models
lm(nit ~ alt, data=env)

#nitrato = 3.561178 - 0.003961 * altitude + E
summary(lm(nit ~ alt, data=env))

SQDR <- b*(sum(dados$XY) - (sum(dados$X) *sum(dados$Y))/nrow(env))
SQDR
SQDT <- sum(dados$Y^2) - (sum(dados$Y)^2/nrow(env))
SQDT
SQDE <- SQDT - SQDR
SQDE
QMR <- SQDR/1
QMR
QME <- SQDE/(nrow(env)-2)
QME
valorF <- QMR/QME
valorF  
Fcritico <- qf(0.095,1,(nrow(env)-2))
Fcritico
pvalue <- 1 - pf(valorF,1,(nrow(env)-2))
pvalue

AnovaReg <-data.frame(Causas_variacao=c("Regressao","Erro","Total"),
                      GL=c(1,(nrow(env)-2),nrow(env)-1),
                      SQD=round(c(SQDR, SQDE, SQDT),3),
                      QM = c(round(QMR, 3), round(QME,3),"-"),
                      Valor_F=c(round(valorF,3),"-","-"),
                      Valor_p=c(round(pvalue,9),"-","-"))
AnovaReg
Coef_det <- SQDR/SQDT
Coef_det

anova(lm(nit ~ alt, data=env))
mod1 <- lm(nit ~ alt, data=env)
names(mod1)

mod1$coefficients
View(data.frame(Residuals=mod1$residuals,
                Fitted=mod1$fitted.values))
x11()
plot(x=mod1$fitted.values,y=mod1$residuals,xlab="Fitted values", ylab="Residuals")
par(mfrow=c(2,2))
plot(mod1)

par(mfrow=c(1,1))
plot(env$alt,env$nit,xlab="Altitude (m)",ylab="Nitrato (mg/L)",pch=19,col="red")
abline(mod1,lwd=2,col="red")

#regressao multipla
mod2 <- lm(nit ~ alt+deb, data=env)
summary(mod2)
anova(mod2)
mod2$coefficients
