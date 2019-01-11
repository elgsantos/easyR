#Mudar caminho pasta
setwd("~/EasyR")
getwd()

#Ler csv sep separador, dec decimal
env <- read.csv("~/EasyR/Doubs.csv",header=T,sep=",",dec=".")
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
