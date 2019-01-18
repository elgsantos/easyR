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