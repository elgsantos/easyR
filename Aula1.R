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



