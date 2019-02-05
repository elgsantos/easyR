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

#homocedasticidade das variancias, deve ser feito antes da ANOVA
#H0 : igualdade das variancias
#Teste de Barlett
#tende a mascarar diferencas quando a curtose é negativa
#encontra dif que não esxistem com curtose positiva
bartlett.test(env$oxy ~ env$zona)

#Rbook sugere -> teste de Fligner-Killen
fligner.test(env$oxy ~ env$zona)

#Teste Levene
#pacotes
#install.packages("car")
library(car)
leveneTest(oxy ~ zona,data=env)
leveneTest(oxy ~ zona,data=env,center=mean)

#ANOVA na mão
gl<-length(levels(env$zona))-1 #grau de liberdade fator (k-1)
gl
glt<-length(env$oxy)-1 #grau de liberdade total (n-1)
glt
glr<-glt - gl #(n -1) - (k-1)
glr

correcao<-sum(env$oxy)^2/length(env$oxy)
correcao

#soma dos quadrados dos desvios - SQD
SQT<-sum(env$oxy^2) - correcao
SQT
tapply(env$oxy,env$zona,length)
#as repeticoes nao sao iguais
#SQF = (soma Brema(6.3 + ... + 8.2)^2 / r Brema +
#       soma Barbo(10.3 + ... + 9.1)^2 / r Barbo +
#       soma Grayling(11.5 + ... + 10.2)^2 / r Grayling +
#       soma Truta(12.2 + ... + 10.0) )^2 / r  Truta) - C 

SQF <- tapply(env$oxy,env$zona,sum)
SQF
SQF <- SQF^2
SQF
SQF <- SQF/tapply(env$oxy,env$zona,length)
SQF <-sum(SQF)
SQF <- SQF-correcao
SQF

#SQD do erro
SQR <- SQT - SQF
SQR

QMF <- SQF/gl
QMF
QMR <- SQR/glr
QMR
valorF <- QMF/QMR
valorF
Fcritico <- qf(0.95,gl,glr)# função dos quartis da distribuição F, probabilidade, gl / glr 
Fcritico #rejeita H0
pvalue <- 1 - pf(valorF,gl,glr)# função da distribuição de F, subtrai de 1, obtém-se o p 
pvalue 

TabelaAnova <- data.frame(Causas_variacao=c("Zona","Resíduos","Total"),
                          GL=c(gl,glr,glt),
                          SQD=round(c(SQF,SQR,SQT),2),
                          QM=c(round(QMF,2),round(QMR,2),"-"),
                          Valor_F=c(round(valorF,2),"-","-"),
                          Valor_p=c(round(pvalue,6),"-","-")) 
TabelaAnova 

#Coef de determinacao R2
Coef_determ <- SQF/SQT
Coef_determ # aproximadamente 62% da variação total foi explicada pelo fator zona 

CV <- sqrt(QMR)/mean(env$oxy)*100 
CV

#independencia dos residuos
mzona <- tapply(env$oxy,env$zona,mean)
mzona
res_oxy <- c(env$oxy[env$zona=="Truta"]-mzona[4],
            env$oxy[env$zona=="Grayling"]-mzona[3],
            env$oxy[env$zona=="Barbo"]-mzona[1],
            env$oxy[env$zona=="Brema"]-mzona[2]
        )
res_oxy

res_pad <- res_oxy / sqrt(QMR) 
res_pad 

jpeg("analise_res.jpg", width=200, height=80, bg="white", res=300, unit="mm") 
par(mfrow=c(1,2),mar=c(4,5,1,1)) 
plot(res_oxy, ylab="Resíduos",cex=1.2) 
plot(res_pad,  ylab="Resíduos padronizados",cex=1.2,ylim=c(-2,2))
abline(h=0, lty=2) 
dev.off()

length(res_pad[(res_pad>-1)&(res_pad<1)])/length(res_pad)*100
length(res_pad[(res_pad>-2)&(res_pad<2)])/length(res_pad)*100

#anova
anova(aov(oxy ~ zona, data=env))
TabelaAnova

x11()
par(mfrow=c(2,2))
plot(aov(oxy ~ zona, data=env))

jpeg("analise_res_aov_amm.jpg", width=420, height=220, bg="white", res=300, 
     unit="mm") 
par(mfcol=c(2,4),cex.main=1.2,pch=21,cex=1.2) 
plot(aov(oxy ~ zona, data=env),main="Oxigênio",bg="lightblue") 
plot(aov(amm ~ zona, data=env),main="Amônio",bg="orange") 
dev.off() 

#teste post-hoc - pairwise comparisons - Teste de Tukey
#diferenca honestamente significante
#honestly significant difference - HSD

TukeyHSD(aov(oxy ~ zona, data=env),ordered=T)

library(gplots)
media<- tapply(env$oxy,env$zona,mean)
desvio<- tapply(env$oxy,env$zona,sd)
par(mfrow=c(1,1))
barplot2(media,col="lightblue", ylab="Oxygênio (mg/L)", ylim=c(0,14),  
         las=1, plot.ci=TRUE,ci.u=media+desvio,ci.l=media-0) 
par(new=T) 
plot(tapply(env$oxy,env$zona,mean),type="n",xlim=c(0.5,4.5),axes=F, 
     ylim=c(0,14),xlab="",ylab="") 
text(x=media+desvio,labels=c("a","b","a","a"),pos=3,offset=0.8)

pen2 <- rep("very_steep", nrow(env))  
quantile(env$pen) 
pen2[env$pen <= quantile(env$pen)[4]] = "steep" 
pen2[env$pen <= quantile(env$pen)[3]] = "moderate" 
pen2[env$pen <= quantile(env$pen)[2]] = "low" 
pen2 <- factor(pen2, levels=c("low", "moderate", "steep", "very_steep")) 
table(pen2)

env$pen2 <- pen2 
media2 <- tapply(env$oxy, env$pen2, mean) 
desvio2 <- tapply(env$oxy, env$pen2, sd) 
barplot2(media2,col="lightblue", ylab="Oxygênio (mg/L)", ylim=c(0,14), 
         las=1, plot.ci=TRUE,ci.u=media2+desvio2,ci.l=media2-0)

anova(aov(oxy ~ zona + pen2, data=env))
TukeyHSD(aov(oxy ~ zona + pen2, data=env),order=T)$pen2
leveneTest(oxy ~ pen2, data=env)