#Mudar caminho pasta
setwd("~/EasyR/minicurso")
getwd()

#Ler csv, header(cabecalho = T ou True), sep separador, dec decimal
env <- read.csv("~/EasyR/minicurso/Doubs.csv",header=T,sep=",",dec=".")
env

colnames(env)
colnames(env)[1] <- "sample"
names(env)

#zona e regiao

#análise exploratória para identificar a região
#utilizar oxigênio, nitrato e amônio x das(km)
#altitute(m) x das (km)

env$zona <- rep(x="Truta",times=nrow(env))
env$zona[env$das>100] <- "Grayling"
env$zona[env$das>200] <- "Barbo"
env$zona[env$das>300] <- "Brema"

env$regiao <- rep("Salmonideo", times=nrow(env))
env$regiao[env$das>200] <- "Ciprinideo"

amm <- env$amm
nit <- env$nit
oxy <- env$oxy

media <- function(variavel){
  sum(variavel)/length(variavel)
}

media(oxy)
oxyNA <- oxy
#ultimo valor substituir por NA
oxyNA[30] <- NA
#media
mean(oxyNA)
#remover valores NA
mean(oxyNA, na.rm = TRUE)

#mediana: conj de dados ordenados, divide em dois subconj
#com mesmo n de elementos

#ordenar
oxy2 <- oxy[order(oxy)]
oxy2

#mediana = media(n/2,n/2+1)
#n/2 = 15, n/2+1=16
E15 <- oxy2[length(oxy2)/2]
E16 <- oxy2[length(oxy2)/2+1]
mean(E15,E16)
#funcao da mediana
median(oxy)

#separatrizes = quartis
# 1 quartil, 2 quartil mediana, 3 quartil
summary(oxy)

#medidas de dispersao: SQD e variancia, desvio padrao, cv,
#erro padrao e intervalo de confianca

#variancia = representa desvio em torno da media
variancia <- data.frame(oxigenio=oxy)
variancia$med_oxy <- rep(mean(oxy),times=length(oxy))
variancia$variacao <- variancia$oxigenio-variancia$med_oxy
#visualiza dataframe
View(variancia)
round(sum(variancia$variacao))
#quadrados do desvio
variancia$QD <- (variancia$oxigenio-variancia$med_oxy)^2
View(variancia)
SQD <- sum(variancia$QD)
SQD #soma dos quadrados do desvio
SQD/(nrow(variancia)-1) #dividido por graus de liberdade (n-1)
#funcao pronta do R
var(oxy) #valor elevado ao quadrado
#como nao ficou muito claro essa comparacao com a variancia
#foi criado o desvio padrao = raiz quadrada da variancia
sqrt(var(oxy))
# ou funcao do R
sd(oxy)
# media 9.39 +/- 2.21 desvio

desvio <- c(sd(nit),sd(amm),sd(oxy))
desvio
x11()
barplot(height = desvio,ylim=c(0,4),
        names.arg=c("nitrato","amônio","oxigênio"))
#desvio padrao nao é o melhor para mostrar dispersao de dados

#coeficiente de variação - CV:dispersao em termos relaticos a media
cv <- c(sd(nit)/mean(nit) *100,sd(amm)/mean(amm) *100,sd(oxy)/mean(oxy) *100)
cv
par(mfrow=c(1,2))
barplot(height = desvio,ylim=c(0,4),ylab="Desvio padrão (mg/L)",
        names.arg=c("nitrato","amônio","oxigênio"))
barplot(height = cv,ylim=c(0,200),ylab="Coeficiente de variação",
        names.arg=c("nitrato","amônio","oxigênio"))

#erro padrao: medida de variacao de uma media amostral em
#relacao a media da populacao: dp/raiz(n)
se_oxy <- sd(oxy)/sqrt(length(oxy))
se_oxy #standard error

#estimar intervalo de confianca para media
#distribuicao Normal padrao media =0 e desvio =1 
#percentil associado ao alfa=5% é igual a 1.96
ICs = mean(oxy)+1.96*se_oxy #intervalo confianca superior 
ICi = mean(oxy)-1.96*se_oxy #inferior
ICs
ICi
mean(oxy)
#10.18 e 8.59 IC com probabilidade de 95% de conter a media
media <- c(mean(nit), mean(amm), mean(oxy))
media
desvio
erroPad <- c(sd(nit)/sqrt(length(nit)),sd(amm)/sqrt(length(amm)),sd(oxy)/sqrt(length(oxy)))