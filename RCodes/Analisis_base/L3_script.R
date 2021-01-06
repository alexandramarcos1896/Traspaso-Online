###################
#NUMERO DE CAMBIOS#
###################

rm(list=ls())

"Librerias a utilizar"
options(warn=0)
library(tidyverse)
#library(sqldf)
library(readxl)
library(dplyr)
library(stringr)
library(ggplot2)
options(warn=2)

"Directorio"
rm(list=ls())
directorio = dirname(rstudioapi::getSourceEditorContext()$path)
setwd(directorio)
dirmother = dirname(getwd())

"Tabla del proceso anterior"

load("./Estadisticos_precios_01.RData")
load("../Base/Base_Final_Peru_completa_01.RData")

BASEDATOS <- base_ll
#Cambios
#(4) Creacion del indicador
#-----------------------------------------
#Primero creamos una variable que contenga la diferencia del precio de internet contra su variable rezagada.
#Esto se almacena en evento_internet
BASEDATOS$Evento_internet <- NA
BASEDATOS$Evento_internet[2:nrow(BASEDATOS)] <- BASEDATOS$precio_internet_completo[2:nrow(BASEDATOS)]-
  BASEDATOS$precio_internet_completo[1:nrow(BASEDATOS)-1]

#Arreglamos la variable anterior pra que las diferencias se realicen a cada item
BASEDATOS$Indicador <-NA
BASEDATOS$Indicador[2:nrow(BASEDATOS)] <- BASEDATOS$ID[2:nrow(BASEDATOS)]-
  BASEDATOS$ID[1:nrow(BASEDATOS)-1]
BASEDATOS$Evento_internet[BASEDATOS$Indicador!=0]<-NA

#Evento es una variable que toma el valor de 1 cuando ha existido un cambio de precios y 0 cuando no.
BASEDATOS$Evento_internet[BASEDATOS$Evento_internet!=0]<-1

cambios = BASEDATOS %>% group_by(ID) %>%
  summarise(numero_cambios = sum(Evento_internet, na.rm = TRUE))

cambios = merge(cambios, Estadisticos[c(1,11:14)])
"Agregamos el porcentaje de cambios"
Estadisticos$Cambios_Obs=cambios$numero_cambios/cambios$PInt_count

#-----------------------------
#GRAFICOS PRESENTACION
#-----------------------------
theme_set(theme_bw())
ggplot(data = Estadisticos, aes(x = Cambios_Obs))+
  geom_histogram(color="darkblue", fill="lightblue") +
  geom_vline(aes(xintercept=mean(Cambios_Obs)),
             color="red", linetype="dashed", size=1) +
  labs(x = "Porcentaje del cambio")


#Primero creamos una variable que contenga la diferencia del precio de internet contra su variable rezagada.
#Esto se almacena en evento_internet
BASEDATOS$Evento_normal <- NA
BASEDATOS$Evento_normal[2:nrow(BASEDATOS)] <- BASEDATOS$precio_normal_completo[2:nrow(BASEDATOS)]-
  BASEDATOS$precio_normal_completo[1:nrow(BASEDATOS)-1]

#Arreglamos la variable anterior pra que las diferencias se realicen a cada item
BASEDATOS$Indicador <-NA
BASEDATOS$Indicador[2:nrow(BASEDATOS)] <- BASEDATOS$ID[2:nrow(BASEDATOS)]-
  BASEDATOS$ID[1:nrow(BASEDATOS)-1]
BASEDATOS$Evento_normal[BASEDATOS$Indicador!=0]<-NA

#Evento es una variable que toma el valor de 1 cuando ha existido un cambio de precios y 0 cuando no.
BASEDATOS$Evento_normal[BASEDATOS$Evento_normal!=0]<-1

cambios = BASEDATOS %>% group_by(ID) %>%
  summarise(numero_cambios = sum(Evento_normal, na.rm = TRUE))

cambios = merge(cambios, Estadisticos[c(1,11:14)])
"Agregamos el porcentaje de cambios"
Estadisticos$Cambios_Obs_normal=cambios$numero_cambios/cambios$PNormal_count
#-----------------------------
#GRAFICOS PRESENTACION
#-----------------------------

theme_set(theme_bw())
ggplot(data = Estadisticos, aes(x = Cambios_Obs_normal))+
  geom_histogram(color="darkblue", fill="lightblue") +
  geom_vline(aes(xintercept=mean(Cambios_Obs_normal)),
             color="red", linetype="dashed", size=1) +
  labs(x = "Porcentaje del cambio")

#---------------------------------------------------
