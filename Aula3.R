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

#ANOVA na mão
#TERMINAR
gl<-length(env$zona)-1
gl
glt<-length(env$oxy)-1
glr<-glt - gl
glr

correcao<-sum(env$oxy)^2/length(env$oxy)
correcao

#soma dos quadrados dos desvios - SQD
SQT<-sum(env$oxy^2)-correcao
SQT
tapply(env$oxy,env$zona,length)

SQF <- tapply(env$oxy,env$zona,sum)
SQF
SQF <- SQF^2
SQF <- SQF/tapply(env$oxy,env$zona,length)
SQF <-sum(SQF)
SQF <- SQF-correcao

SQR <- SQT - SQF
SQR

QMF <- SQF/gl
QMF
QMR <- SQR/glr
QMR

#Coef de determinacao R2
coef_determ <- SQF/SQT
coef_determ

#independencia dos residuos
mzona <- tapply(env$oxy,env$zona,mean)
mzona
res_oxy <- c(env$oxy[env$zona=="Truta"]-mzona[4],
            env$oxy[env$zona=="Grayling"]-mzona[3],
            env$oxy[env$zona=="Barbo"]-mzona[1],
            env$oxy[env$zona=="Brema"]-mzona[2]
        )
res_oxy

#anova
anova(aov(oxy ~ zona, data=env))

x11()
par(mfrow=c(2,2))
plot(aov(oxy ~ zona, data=env))

#teste post-hoc - pairwise comparisons - Teste de Tukey
#diferenca honestamente significante
#honestly significant difference - HSD

TukeyHSD(aov(oxy ~ zona, data=env),ordered=T)

library(gplots)
media<- tapply(env$oxy,env$zona,mean)
desvio<- tapply(env$oxy,env$zona,sd)
par(mfrow=c(1,1))
barplot2(media,col="lightblue",ylab="Oxigênio (mg/L)", ylim=c(0,14),
        las=1,plot.ci=T,ci.u=media+desvio,ci.l=media-desvio)
