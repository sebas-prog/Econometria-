
###########################################################################
#                   Optimización de portafolios 
#       Autor: Sebstián Sosa Pérez 
###########################################################################

# =========================================================================
#                          Librerias 
# =========================================================================
#install.packages("IntroCompFinR", repos="http://R-Forge.R-project.org")

library(IntroCompFinR)
library(pacman)
library(PerformanceAnalytics)
library(quantmod)
library(tidyverse)
library(hrbrthemes)
library(kableExtra)
library(gplots)
library(scales)

# Borrar datos: 

rm(list = ls())

# =========================================================================
#                     Implementación del código 
# =========================================================================

tickers<-c("IBM","BABA","AMZN","FB","MSFT","LNVGY","TSLA")

precios<-NULL

fecha<-"2015-01-01"

for(a in tickers){
  precios <- cbind(precios, getSymbols.yahoo(a, from=fecha , periodicity = "daily" , auto.assign=F)[,6])
}

names(precios)<-tickers

chart.TimeSeries(scale(precios),legend.loc = "topleft")

mean<- apply(Return.calculate(precios,method = "log")[-1,],
             2,function(x) mean(x))

sd<- apply(Return.calculate(precios,method = "log")[-1,],
           2,function(x) sd(x))

m<- as.data.frame(mean)

m<- t(m)

s<- t(as.data.frame(sd))

x<- rbind(m,s)*100

x %>% t() %>% kbl(caption = "Table 1: Media & desviación estandar de los activos", escape = TRUE) %>%
  kable_classic(full_width = FALSE, html_font = "Cambria")

g1<- ggplot(mapping = aes(sd,mean,
                          label=c("IBM","BABA","AMZN","FB","MSFT","LNVGY","TSLA")))+
  geom_point()

g1<- g1+geom_text(hjust=0,vjust=0)

g1<- g1+theme_bw()+xlab("Riesgo")+
  ylab("Retorno")

g2<- g1

g2<-g2+scale_y_continuous(
  breaks = seq(0,0.003,by=0.001),
  limits = c(0,0.003)
)

g2<- g2+scale_x_continuous(
  breaks = seq(0.015,0.037,by=0.007),
  limits = c(0.015,0.037)
)

g2<- g2+ggtitle("Trade-off Riesgo-Retorno",
                subtitle = "7 Activos Riesgosos")+theme_minimal()

g2

retornos<- Return.calculate(precios,method = "log")[-1,]

cov<-cov(retornos)*100

cov %>% round(digits = 4) %>% kbl(caption = "Table 2: Matriz de covarianzas de los activos", escape = TRUE) %>%
  kable_classic(full_width = FALSE, html_font = "Cambria")

generate_heat_map <- function(correlationMatrix, title)
{
  
  heatmap.2(x = correlationMatrix,    
            cellnote = correlationMatrix,   
            main = title,           
            symm = TRUE,            
            dendrogram="none",      
            Rowv = FALSE,           
            trace="none",           
            density.info="none",        
            notecol="black")          
}

corr1 <- round(cor(retornos) * 100, 2)

generate_heat_map(corr1,"Heatmap: Correlaciones")

mean<- apply(retornos,2,function(x) mean(x))

sd<- apply(retornos, 2,function(x) sd(x))

cov<- cov(retornos)

# Pesos  ------------------------------------------------------------------

weights<- rep(1,7)/7


# construimos el portafolio -----------------------------------------------

getPortfolio(mean,cov,weights = 
               weights)


# Portafolio de minima varianza -------------------------------------------

globalmin=globalMin.portfolio(mean,cov,
                              shorts = F)


# Grafico -----------------------------------------------------------------

g3<- ggplot()+geom_point(
  mapping = aes(globalmin$sd,
                globalmin$er,
                color="1"),size=4
)
g3<- g3+geom_point(mapping = 
                     aes(sd,mean,color="2"),
                   size=4)

g3<- g3+ scale_color_manual(
  "",values = c("blue","purple"),
  labels=c("Min Var.","Stocks 1")
)
g3<- g3+ xlab("Riesgo")+ylab("Retorno")+
  ggtitle("Trade-off Riesgo-Retorno",
          subtitle = "Siete Activos riesgos & minima varianza")+
  theme_minimal()
g3

data=as.data.frame(round(globalmin$weights*100,2))

for(i in  1:7){
  if(data[i,1]==0){
    data[i,1]=NA
  }
  else{
    data[i,1]=data[i,1]
  }
}

names(data)=c("Pesos")

data=data.frame("Acciones"=tickers,"Pesos"=data)

data=na.omit(data)

bp<- ggplot(data = data,aes(x="",y=Pesos,fill=Acciones))+
  geom_bar(width = 1,stat = "identity")
pie<- bp + coord_polar("y",start=0)

aa<-pie +scale_fill_brewer(palette = "Blues")+
  ggtitle("Portafolio de minima varianza")

ata<-aa+geom_text(aes(label=percent(data[,2],scale = 1)),
                  position = position_stack(vjust = 0.4),
                  color="black")+
  theme_minimal()

ata + theme(panel.grid.major = element_line(linetype = "longdash"))+
  theme(plot.title = element_text(family = "Times", 
                                  hjust = 0.50), 
        legend.position = "bottom", 
        legend.direction = "horizontal") +labs(x = "Acciones")

# Portafolio de minima varianza sujeto a un retorno objetivo  -------------

port.ibm<- efficient.portfolio(mean,cov,mean[1],shorts = F)
port.baba<- efficient.portfolio(mean,cov,mean[2],shorts = F)
port.amzn<- efficient.portfolio(mean,cov,mean[3],shorts = F)
port.fb<- efficient.portfolio(mean,cov,mean[4],shorts = F)
port.msft<- efficient.portfolio(mean,cov,mean[5],shorts = F)
port.lngvgy<- efficient.portfolio(mean,cov,mean[6],shorts = F)
port.tsla<- efficient.portfolio(mean,cov,mean[7],shorts = F)
mean.2<- c(port.ibm$er,port.baba$er,port.amzn$er,port.fb$er,port.msft$er,
           port.lngvgy$er,port.tsla$er)
sd.2 <- c(port.ibm$sd,port.baba$sd,port.amzn$sd,port.fb$sd,port.msft$sd,
          port.lngvgy$sd,port.tsla$sd)

# Cáluclo Portafolio tangente ---------------------------------------------

risk_free<- 0.0001

por.tang<- tangency.portfolio(mean,cov,
                              risk_free,shorts = F)

sharpe.ratio= (por.tang$er-risk_free)/por.tang$sd   

# Frontera eficiente  -----------------------------------------------------

eff.front.short<- efficient.frontier(mean,cov,
                                     nport = 50,
                                     alpha.min = -2,
                                     alpha.max = 1.5,shorts =F )
# grafica

g6<- ggplot()+geom_point(mapping = 
                           aes(eff.front.short$sd,
                               eff.front.short$er,color="1"))

g6<- g6+geom_point(mapping = aes(por.tang$sd,
                                 por.tang$er,color="4"),size=4)

g6<- g6 + geom_point(mapping = aes(globalmin$sd
                                   ,globalmin$er,color="5"),size=4)

g6<- g6+geom_abline(intercept = risk_free,
                    slope=sharpe.ratio,color="#58FAF4",lty=2,size=1)

g6<- g6+ggtitle("Frontera Eficiente y Recta tangente")+xlab("Riesgo")+ylab("Retorno")+
  theme_minimal()

g6<- g6+scale_color_manual("",
                           values = c(
                             "#0040FF","#FE2EF7","#BDBDBD"      
                           ),
                           labels=c("Frontera Eficiente","Portafolio Tangente",
                                    "Min.var"))

g6

data=as.data.frame(round(por.tang$weights*100,2))

for(i in  1:7){
  if(data[i,1]==0){
    data[i,1]=NA
  }
  else{
    data[i,1]=data[i,1]
  }
}

names(data)=c("Pesos")

data=data.frame("Acciones"=tickers,"Pesos"=data)

data=na.omit(data)

bp<- ggplot(data = data,aes(x="",y=Pesos,fill=Acciones))+
  geom_bar(width = 1,stat = "identity")

pie<- bp + coord_polar("y",start=0)
aa<-pie +scale_fill_brewer(palette = "Blues")+
  ggtitle("Portafolio tangente")
ata<-aa+geom_text(aes(label=percent(data[,2],scale = 1)),
                  position = position_stack(vjust = 0.4),
                  color="black")+
  theme_minimal()

ata + theme(panel.grid.major = element_line(linetype = "longdash"))+
  theme(plot.title = element_text(family = "Times", 
                                  hjust = 0.50),
        legend.position = "bottom", 
        legend.direction = "horizontal") +labs(x = "Acciones")

