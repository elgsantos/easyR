setwd("~/EasyR/minicurso")
getwd()
#tabela criada env_aula2.csv
env <- read.csv("env_aula2.csv",header=T,sep=",",dec=".")
env
env$regiao

jpeg("barras_oxy.jpg", width = 200, height = 60, bg = "white", res=300,unit="mm")
par(mfrow=c(1,3),mar=c(3,4.5,3,1))
barplot(height = tapply(env$oxy,env$zona,mean),col="lightblue",ylim=c(0,14),
        ylab="Oxigênio (mg/L)",las=1, main="Média")
barplot(height = tapply(env$oxy,env$zona,var),col="lightblue",ylim=c(0,4),
        las=1, main="Variância")

cv <- tapply(env$oxy,env$zona,sd)/tapply(env$oxy,env$zona,mean)*100

barplot(height = cv,col="lightblue",ylim=c(0,25),
       las=1, main="Coeficiente de variação")

dev.off()

#homocedasticidade das variancias
#Teste de Barlett
#tende a mascarar diferencas quando a curtose é negatica
#encontra dif que não esxistem com curtose positiva
bartlett.test(env$oxy ~ env$zona)

#Rbook sugere -> teste de Fligner-Killen
fligner.test(env$oxy ~ env$zona)

#Teste Levene
#pacotes
install.packages("car")
library(car)
leveneTest(oxy ~ zona,data=env)
leveneTest(oxy ~ zona,data=env,center=mean)

