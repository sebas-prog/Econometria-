########################################################################
#                        Tema: Diagrama de Pareto
#         Autor: Sebastián Sosa Pérez
########################################################################

#############################################
# Eliminar datos
#############################################
rm(list = ls())

#############################################
# Carga los paquetes
#############################################
library(ggplot2)
library(qcc)
library(readr)
library(hrbrthemes)
# ojo: install.packages("qqc")

############################################
# Código a utilizar 
############################################
data<- read.csv("https://raw.githubusercontent.com/sebas-prog/Rocio-Chavez-youtube-Files/master/Datos%20Compras.csv")
data<- table(data$Producto)
barplot(data,width = 1)
pareto.chart(data)
ata<-pareto.chart(data)
ata<-as.data.frame(sort(data,decreasing = T))
ggplot(ata,aes(x=Var1,y=Freq))+geom_bar(stat="identity", position="stack",mapping = aes(fill=Var1))
ata$"acum"<- ata$Freq/sum(ata$Freq)*100
ata$"acuma"<- NA
a<-0
for(i in 1:length(ata$Var1)){
  a<- a+ata$acum[i]
  ata$acuma[i]<-a
  }
ggplot(ata,aes(x=Var1,y=acum))+
  geom_bar(stat="identity", position="stack",mapping = aes(fill=Var1),
           alpha=0.5)+
  geom_point(mapping = aes(y=ata$acuma),show.legend = T,color="blue")+
  geom_line(aes(y=ata$acuma, group=), group = 1,color="#58FAF4") + 
  theme_modern_rc()
