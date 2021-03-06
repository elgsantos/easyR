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
erroPad
cv
CIs <- media + 1.96 * erroPad
CIs
CIi <- media - 1.96 * erroPad
CIi
tabela <- data.frame(media=media, dp=desvio, ep=erroPad,
                     CV=cv, ICs=CIs, ICi=CIi)
tabela
row.names(tabela) <- c("nitrato","amonio","oxigenio")
tabela

#Como saber, por exemplo qual a media do oxigenio para uma determinada região
env$regiao
env$zona

x11()
par(mfrow=c(2,1),mar=c(3,4.5,1,1))
boxplot(oxy ~ regiao, data=env, col="lightblue", ylab="Oxigênio (mg/L)")
boxplot(oxy ~ zona, data=env, col="lightblue", ylab="Oxigênio (mg/L)")

tapply(X=env$oxy, INDEX=env$regiao, FUN = mean)
tapply(X=env$oxy, INDEX=env$zona, FUN = mean)

tapply(X=env$oxy, INDEX=env$regiao, FUN = length)#13 | 17 desenho amostral desbalanceado
tapply(X=env$oxy, INDEX=env$zona, FUN = length)

tapply(env$oxy, list(env$regiao,env$zona), mean)
#desenho amostral aninhado (nested)
# salmonídeo contem 2 zonas e ciprinideo contem 2 zonas diferentes
# Regiao:   Salmonideo      Ciprinideo
# Zona:   Truta Grayling    Brema Barbo  #estao aninhados nas regioes

#tabela dinamica
aggregate(env[,9:11],list(regiao=env$regiao),mean)
aggregate(env[,9:11],list(zona=env$zona),mean)
#juntando
aggregate(env[,9:11],list(regiao=env$regiao,zona=env$zona),mean)

tabela2 <- aggregate(env[,9:11],list(zona=env$zona),mean)
tabela2

# regiao: Ciprinídeo e Salmonídeo
par(mfrow=c(1,1))
barplot(tapply(env$oxy, env$regiao, var), ylim=c(0,5))

#teste de Fisher = compara duas variancias
#hipotese nula de que as variancias nao diferem

var.test(env$oxy[env$regiao=="Ciprinideo"],
         env$oxy[env$regiao=="Salmonideo"])
#aceita hipotese nula p-value < 0.5

#Teste de Shapiro-Wilk - normalidade
#nitrato e amonio - nao apresentaram distribuicao normal
#Teste de Wilcoxon - nao parametrico
dados_wil <- aggregate(env[,9:10],list(regiao=env$regiao),mean)
#ajuste coluna
row.names(dados_wil) <- dados_wil[,1]
dados_wil <- dados_wil[,-1]
dados_wil <- as.matrix(dados_wil)

barplot(height = dados_wil,beside = T,ylim=c(0,3),legend=T)

wilcox.test(env$nit[env$regiao=="Ciprinideo"],
            env$nit[env$regiao=="Salmonideo"])

#rejeita a H0, aceita Ha=medias amm sao diferentes
wilcox.test(env$amm[env$regiao=="Ciprinideo"],
            env$amm[env$regiao=="Salmonideo"])
#rejeita a H0, aceita Ha=medias sao diferentes

#oxy apresentou distribuicao normal
#teste t Student = parametrico

jpeg("Boxplot_oxy_notch.jpg",width = 100, height = 80, bg="white",
     res = 300, unit="mm")
par(mar=c(3,4.5,1,1))
boxplot(oxy ~ regiao, data = env, notch=T, col="lightblue", ylab="Oxigênio (mg/L)")
dev.off()
t.test(env$oxy[env$regiao=="Ciprinideo"],
        env$oxy[env$regiao=="Salmonideo"])
#rejeita H0, aceita Ha

#correlacao entre variaveis: Pearson e Spearman
#valor -1 = correlacao negativa
#valor 1 = corr positiva
#valor 0 = ausencia de correlacao
#nao representa dependencia

jpeg("Graf_correlacao.jpg",width = 260, height = 100, bg="white",
     res = 300, unit="mm")
par(mfrow=c(1,2),mar=c(4.5,4.5,1,4.5))
plot(env$das,env$amm,type="l",xlab="Distância da fonte(km)",
     ylab="Amônio (mg/L)",col="darkorange",lwd=2)
par(new=T)#adicionar novo grafico
plot(env$das,env$oxy,type="l",xlab="", ylab="",col="blue",
     lwd=2, axes=F)
axis(side = 4, col="blue", col.axis="blue")
mtext(text="Oxigênio (mg/L)", side = 4, padj=4, col="blue")

plot(env$das,env$nit,type="l",xlab="Distância da fonte(km)",
     ylab="Nitrato (mg/L)",col="red",lwd=2)
par(new=T)#adicionar novo grafico
plot(env$das,env$amm,type="l",xlab="", ylab="",col="darkorange",
     lwd=2, axes=F)
axis(side = 4, col="darkorange", col.axis="darkorange")
mtext(text="Amônio (mg/L)", side = 4, padj=4, col="darkorange")
dev.off()

cor.test(env$oxy,env$amm, method = "pearson")
#negativamente correlacionadas
cor.test(env$nit,env$amm, method = "spearman")#spearman - nao parametrico
#correlacionados

write.table(x=env, file="env_aula2.csv",row.names = F,sep=",") #remove numeracao das linhas

names(read.csv("env_aula2.csv",header=T,sep=",",dec="."))