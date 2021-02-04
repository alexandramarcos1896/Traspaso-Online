#-------------------------------------------------
#Proceso de filtrado de la base de datos
#-------------------------------------------------

#El objetivo de este script es poder realizar un proceso de limpieza.
#Autores como Cavallo eliminan aquellos productos que presentan cambios 
#de precios fuertes por dia. Este indicador se calcula en este script y posterior
#se eliminan los productos.


#-------------------------
#(1) Limpieza de memoria, librerias
#-------------------------
rm(list=ls())
options(warn=0)
library("dplyr")
library("ggplot2")
library("tidyverse")
library("stringr")
options(warn=2)

#-------------------------
#(2) Directorio de trabajo
#-------------------------
directorio = dirname(rstudioapi::getSourceEditorContext()$path)
setwd(directorio)
dirmother = dirname(getwd())

#-----------------------------------------
#(3) Cargamos bases de datos
#-----------------------------------------
load(paste0(dirmother,"/Base/Base_Final_Evento.RData"))


#-----------------------------------------
#(4) Magnitud del cambio de precio por dia
#-----------------------------------------
#Se toma log a los precios de internet
BASEDATOS$log_precio_inter = log(BASEDATOS$precio_internet)
#Se crea la variable magnitud
BASEDATOS$magnitud <- NA
#Resta del precio - precio rezagado por 100
BASEDATOS$magnitud[2:nrow(BASEDATOS)] = (BASEDATOS$log_precio_inter[2:nrow(BASEDATOS)]-
                                           BASEDATOS$log_precio_inter[1:nrow(BASEDATOS)-1])*100


BASEDATOS$Indicador <- NA #Esta variable nos ayuda a identificar si seguimos en el mismo producto (valor 0) o si 
#nos encontramos en el siguiente (valor 1)

BASEDATOS$Indicador[2:nrow(BASEDATOS)] <- BASEDATOS$ID[2:nrow(BASEDATOS)] -
  BASEDATOS$ID[1:nrow(BASEDATOS)-1]
BASEDATOS$magnitud[BASEDATOS$Indicador!= 0]<-NA #Corregimos los falsos cambios 

#-----------------------------------------
#(5) Identificamos los cambio por encima de 200
#-----------------------------------------
#Identificamos aquellos items que han tenido cambios
#muy brusco y dado que son muy pocos bienes son eliminados de la base cargada.
c <- which(abs(BASEDATOS$magnitud)>160)
d <- BASEDATOS[c,] %>% distinct(ID)
Base_datos_llenar = BASEDATOS %>% filter(ID %in% d$ID)
#Eliminamos
BASEDATOS = anti_join(BASEDATOS,Base_datos_llenar)
rm(Base_datos_llenar,d)

#-----------------------------------------
#(6) Guardamos base
#-----------------------------------------
sv = paste0(dirmother,"/Base/Base_Final_Filtrada.RData")
save(BASEDATOS, file = sv)

