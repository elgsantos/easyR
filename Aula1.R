#Mudar caminho pasta
setwd("~/EasyR/minicurso")
getwd()

#Ler csv sep separador, dec decimal
env <- read.csv("~/EasyR/minicurso/Doubs.csv",header=T,sep=",",dec=".")
env

#dataframe objeto bidimensional que suporta dados de dif tipos
#ver a classe
class(env)
str(env)
#6 primeiras linhas
head(env)
#6 ultimas linhas 
tail(env)

#[linha, coluna]
env[,3]
env[25,]
env[,5:9]
#pulando colunas
env[11:20,c(7,9,11)]
#trocar nome coluna
colnames(env)[1] <- "sample"

#numero
nrow(env)
ncol(env)
ncol(env[,-1])
dim(env)
summary(env[8:10])

range(env)
range(env$nit)
max(env$nit)

#tipologia 4 zonas, partindo da nascente
#zona da truta, salmo trutta fario
#zona do grayling, thymallus thymallus
#zona do barbo, brabus barbus
#zona da brema, abramis brama

#2primeiros
#Regiao dos Salmonídeos: bem oxigenada e oligotrófica
#2ultimos
#Regiao dos Ciprinideos: eutrófica e baixa concentração de oxy

#análise exploratória para identificar a região
#utilizar oxigênio, nitrato e amônio x das(km)
#altitute(m) x das (km)

env$zona <- rep(x="Truta",times=nrow(env))
env$zona[env$das>100] <- "Grayling"
env$zona[env$das>200] <- "Barbo"
env$zona[env$das>300] <- "Brema"

env$regiao <- rep("Salmonideo", times=nrow(env))
env$regiao[env$das>200] <- "Ciprinideo"


#funcao para salvar figura
jpeg("~/EasyR/minicurso/Variaveis.jpg",width = 200,height = 200, bg="white", res=300,
     units="mm")
#par - funcao de parametros graficos
#multiframerow mfrow
par(mfrow=c(2,2),mar=c(4.5,4.5,1,1))#inferior,esq,sup,dir
plot(x=env$das,y=env$nit,type="l",xlab="distância da fonte(km)",
     ylab="Nitrato (mg/L)",col="red",lwd=2)
plot(x=env$das,y=env$amm,type="l",xlab="distância da fonte(km)",
     ylab="Amônio (mg/L)",col="green",lwd=2)
plot(x=env$das,y=env$oxy,type="l",xlab="distância da fonte(km)",
     ylab="Oxigênio (mg/L)",col="orange",lwd=2)
plot(x=env$das,y=env$alt,type="l",xlab="distância da fonte(km)",
     ylab="Altitude (m))",col="blue",lwd=2)
points(x=env$das[env$zona=="Truta"],y=env$alt[env$zona=="Truta"],
       pch=19,col="black")
points(x=env$das[env$zona=="Grayling"],y=env$alt[env$zona=="Grayling"],
       pch=19,col="gray40")
points(x=env$das[env$zona=="Barbo"],y=env$alt[env$zona=="Barbo"],
       pch=19,col="gray70")
points(x=env$das[env$zona=="Brema"],y=env$alt[env$zona=="Brema"],
       pch=21,col="black",bg="white")
legend("topright",legend=c("Truta","Grayling","Barbo","Brema"),
       bty="n",pch=c(19,19,19,21),col=c("black","gray40","gray70","black"),
       pt.cex=1.2,title="Zonas")
abline(v=200,lty=3,col="darkorange",lwd=2)
text(x=200,y=300,labels="Região \nSalmonídeo",pos=2, col="darkorange")
text(x=200,y=400,labels="Região \nCiprinídeo",pos=4, col="darkorange")

#salvar
dev.off()

#----------------------------------------------------
#boxplot ou diagrama de caixa
#box-and-whisker
par(mar=c(3,3,1,1))
boxplot(env$nit,env$amm,env$oxy,names=c("Nitrato","Amônio","Oxigênio"),
        col=c("pink","orange","lightblue"),las=1)
boxplot(env$nit,env$amm,names=c("Nitrato","Amônio"),
        col=c("pink","orange"),las=1,outline=FALSE)

jpeg("~/EasyR/minicurso/VariaveisRegiao.jpg",width = 200,height = 60, bg="white", res=300,
     units="mm")
par(mfrow=c(1,3),mar=c(3,4.5,1,1))
#nitrato em funcao da regiao
boxplot(nit ~ regiao,data=env,col="pink",ylab="Nitrato (mg/L)",las=1)
boxplot(amm ~ regiao,data=env,col="orange",ylab="Amônio (mg/L)",las=1)
boxplot(oxy ~ regiao,data=env,col="lightblue",ylab="Oxigênio (mg/L)",las=1)
dev.off()

#análises paramétricas e não-paramétricas
#distribuição normal dos erros da variável

n100 <- rnorm(n=100)
n1k <- rnorm(n=1000)
n10k <- rnorm(n=10000)

#histograma de frequencia e grafico quantile-quantile
#distribuição normal - tende a reta da distrib teorica
#caso contrario - formato S ou de uma banana

x11()
par(mfcol=c(2,3),mar=c(4.5,4.5,3,1),cex.lab=1.6,cex.axis=1.4,
    cex.main=1.8)
hist(n100,col="gray",main="Histograma n=100", xlab = "Dados simulados")
qqnorm(n100,pch=21,col="black",bg="gray",cex=1.6)
qqline(n100,lty=2)
hist(n1k,col="gray",main="Histograma n=1000", xlab = "Dados simulados")
qqnorm(n1k,pch=21,col="black",bg="gray",cex=1.6)
qqline(n1k,lty=2)
hist(n10k,col="gray",main="Histograma n=10000", xlab = "Dados simulados")
qqnorm(n10k,pch=21,col="black",bg="gray",cex=1.6)
qqline(n10k,lty=2)

jpeg("~/EasyR/minicurso/Normalidade.jpg",width = 400,height = 200, bg="white", res=300,
     units="mm")
par(mfcol=c(2,4),mar=c(5,4.5,3,1),cex.lab=1.6,cex.axis=1.4,
    cex.main=1.8)
hist(env$nit,col="pink",main="Histograma de Nitrato", xlab = "Nitrato (mg/L)")
qqnorm(env$nit,pch=21,col="black",bg="pink",cex=1.6)
qqline(env$nit,lty=2)
hist(env$amm,col="orange",main="Histograma de Amônio", xlab = "Amônio (mg/L)")
qqnorm(env$amm,pch=21,col="black",bg="orange",cex=1.6)
qqline(env$amm,lty=2)
hist(env$oxy,col="gray",main="Histograma de Oxigênio", xlab = "Oxigênio (mg/L)")
qqnorm(env$oxy,pch=21,col="black",bg="gray",cex=1.6)
qqline(env$oxy,lty=2)

hist(n100,col="gray",main="Histograma n=100", xlab = "Dados simulados")
qqnorm(n100,pch=21,col="black",bg="gray",cex=1.6)
qqline(n100,lty=2)
dev.off()

#teste de normalidade Shapiro-Wilk
#hipótese nula (h0) é que os dados da variável possuem distribuição normal
#prob p>0.05, h0 é aceita, caso contrário p<0.05 rejeita h0 (não normais)

shapiro.test(n100)
#p>0.05 aceita h0

shapiro.test(env$nit)
#p<0.05 rejeita h0 (não é normal)

shapiro.test(env$amm)
#p<0.001 rejeita H0

shapiro.test(env$oxy)
#p>0.05 aceita H0