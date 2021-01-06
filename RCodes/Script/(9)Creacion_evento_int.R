#------------------------------------------
#Creacion variable Evento Internet
#------------------------------------------

#(1)Limpiar memoria y cargar librerias
#----------------------------------------
rm(list=ls())
options(warn=0)
library(tidyverse)
library(dplyr)
library(data.table)
library(stringr)
options(warn=2)

#(2) Directorio de trabajo
#----------------------------------------
directorio = dirname(rstudioapi::getSourceEditorContext()$path)
setwd(directorio)
dirmother = dirname(getwd())


#(3) Base de Datos
#-----------------------------------------
load(paste0(dirmother,"/Base/PreBase(Actualizacion)/Base_Final_Peru_2.RData"))



#(4) Creacion del indicador
#-----------------------------------------
#Primero creamos una variable que contenga la diferencia del precio de internet contra su variable rezagada.
#Esto se almacena en evento_internet
BASEDATOS$Evento_internet <- NA
BASEDATOS$Evento_internet[2:nrow(BASEDATOS)] <- BASEDATOS$precio_internet[2:nrow(BASEDATOS)]-
  BASEDATOS$precio_internet[1:nrow(BASEDATOS)-1]

#Arreglamos la variable anterior pra que las diferencias se realicen a cada item
BASEDATOS$Indicador <-NA
BASEDATOS$Indicador[2:nrow(BASEDATOS)] <- BASEDATOS$ID[2:nrow(BASEDATOS)]-
  BASEDATOS$ID[1:nrow(BASEDATOS)-1]
BASEDATOS$Evento_internet[BASEDATOS$Indicador!=0]<-NA

#Evento es una variable que toma el valor de 1 cuando ha existido un cambio de precios y 0 cuando no.
BASEDATOS$Evento_internet[BASEDATOS$Evento_internet!=0]<-1

#Borramos indicador
BASEDATOS<- BASEDATOS[-17]

#(5) Guardemos base de datos
#-----------------------------------------
sv = paste0(dirmother,"/Base/Base_Final_Evento.RData")
save(BASEDATOS, file = sv)
