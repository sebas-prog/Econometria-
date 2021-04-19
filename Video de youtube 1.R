###########################################################################
####### Tema: Web Scraping para la extracción de datos del BCRP ###########
####### Autor: Sebastián Sosa Pérez #######################################

# =========================================================================
#                        Librerias a utilizar     
# =========================================================================

library(tidyverse)
library(rvest)

# =========================================================================
#                        Extracción de datos      
# =========================================================================

url<- "https://estadisticas.bcrp.gob.pe/estadisticas/series/mensuales/resultados/PN01728AM/html"
tmp<- read_html(url)
tmp <- html_nodes(tmp, "table")
sapply(tmp, class)
sapply(tmp, function(x) dim(html_table(x, fill = TRUE)))
pbi <- html_table(tmp[[2]])
pbi$Fecha<- seq(as_date("2004-01-01"),length=length(pbi$Fecha),
                by="1 month")
names(pbi)[2]<- "pbi"
head(pbi)
pbi %>% ggplot(aes(Fecha,pbi))+geom_line(color="blue")+
  theme_minimal()+labs(title = "Web Scraping del bcrp")+
  xlab("Tiempo")+ylab("Evolución del PBI")
pbi %>%  mutate(fecha=format(Fecha,"%Y")) %>% 
  group_by(fecha) %>%
  ggplot(aes(x=fecha, y=pbi))+
  geom_boxplot(fill="white", colour="#3366FF", outlier.color = "red")+
  theme_minimal()+
  labs(title="Distribución historica de las variaciones",
       subtitle = "Gráfico de caja",
       caption= "Elaboración propia",
       y="Retornos del IPC",
       x= "Año")

# =========================================================================
#                        Creación de una función     
# =========================================================================

scraping_bcrp<- function(url=NULL){
  url<- url
  tmp<- read_html(url)
  tmp<- html_nodes(tmp,"table")
  datos<- html_table(tmp[2])
  return(data=datos)
}

ata<- scraping_bcrp("https://estadisticas.bcrp.gob.pe/estadisticas/series/mensuales/resultados/PN01728AM/html")

# =========================================================================
#                        Uso de una API
# =========================================================================

scraping_bcrp_omega<- function(codigo=NULL,start=NULL,end=NULL){
  url<-  paste0("https://estadisticas.bcrp.gob.pe/estadisticas/series/api/", 
                codigo, "/html/",start,"/",end)
  tmp<- read_html(url)
  tmp<- html_nodes(tmp,"table")
  datos<- html_table(tmp[2])
  return(datos)
}

ata<- scraping_bcrp_omega("PN01728AM","2000-1","2020-12")