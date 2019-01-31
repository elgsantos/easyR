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