#**********************************************
"Identificar el tamaño del cambio promedio por dia"
#PRECIOS INTERNET
#**********************************************

"Directorio"
rm(list=ls())
graphics.off() #Elimina cualquier grafico
directorio = dirname(rstudioapi::getSourceEditorContext()$path)
setwd(directorio)
dirmother = dirname(getwd())

"Funciones"
options(warn=0)
library(tidyverse)
library(dplyr)
library(data.table)
library(stringr)
options(warn=2)

"Cargamos la base de datos"
#---------------------------
load("../Base/Base_Final_Peru_completa_01.RData")
BASEDATOS <- base_ll
rm(base_ll)

"Se calcula la magnitud del cambio de precio por dia"
#---------------------------
BASEDATOS$log_precio_inter = log(BASEDATOS$precio_internet_completo)
BASEDATOS$magnitud[2:nrow(BASEDATOS)] = (BASEDATOS$log_precio_inter[2:nrow(BASEDATOS)]-BASEDATOS$log_precio_inter[1:nrow(BASEDATOS)-1])*100

BASEDATOS$ID = as.numeric(BASEDATOS$ID)

BASEDATOS$Indicador = NA #Esta variable nos ayuda a identificar si seguimos en el mismo producto (valor 0) o si 
#nos encontramos en el siguiente (valor 1)

BASEDATOS$Indicador[2:nrow(BASEDATOS)] <- BASEDATOS$ID[2:nrow(BASEDATOS)] -BASEDATOS$ID[1:nrow(BASEDATOS)-1]
BASEDATOS$magnitud[BASEDATOS$Indicador!= 0]<-0 #Corregimos los falsos cambios 
BASEDATOS$magnitud[1]<- 0

#******************************************
"Tabla del tamaño del promedio/mediana por dia"
#******************************************

cambios_dia = BASEDATOS %>% group_by(fecha) %>% distinct(fecha)
cambios_dia$magnitud_cambio_promedio = NA
cambios_dia$mediana = NA
#Creamos el loop y el vector de fechas a recorrer
contador = 1 
avector <- cambios_dia[['fecha']]

n= length(avector)
for (ii in 1:n) {
  BD1 = filter(BASEDATOS, fecha == avector[ii])
  cambios_dia$magnitud_cambio_promedio[contador] = BD1 %>%
    filter(!(magnitud ==0 | is.na(magnitud))) %>% summarise(mean(abs(magnitud)))
  cambios_dia$mediana[contador] = BD1 %>%
    filter(!(magnitud ==0 | is.na(magnitud))) %>% summarise(median(abs(magnitud)))
  contador = contador + 1 
  rm(BD1)
}
cambios_dia$magnitud_cambio_promedio = as.numeric(cambios_dia$magnitud_cambio_promedio)
cambios_dia$magnitud_cambio_promedio = round(cambios_dia$magnitud_cambio_promedio,3)
cambios_dia$mediana = as.numeric(cambios_dia$mediana)
cambios_dia$mediana = round(cambios_dia$mediana,3)
#Ordenamos
cambios_dia = cambios_dia[order(cambios_dia$fecha),]

#******************************************
"Grafico del tamaño de cambio diario"
#******************************************

#Grafico de cambios promedio por dia  
theme_set(theme_bw())
ggplot(data = cambios_dia ) + geom_point(aes(x = fecha, y = magnitud_cambio_promedio),na.rm=TRUE, col = "darkblue") + 
  geom_line(aes(x = fecha, y = magnitud_cambio_promedio) ,na.rm=TRUE) +
  scale_x_date(date_breaks = "30 days", date_labels = "%b %d") +
  theme(axis.text  = element_text(angle = 90)) + geom_hline(yintercept = 21.07667, col= "red")

#Grafico de cambios promedio por dia  +  mediana por dia y el promedio total
cutoff <- mean(cambios_dia$magnitud_cambio_promedio, na.rm = TRUE)
xdate <- as.Date("2016-08-19")
ggplot(data = cambios_dia ) + geom_point(aes(x = fecha, y = mediana),na.rm=TRUE, col = "blue") + 
  geom_line(aes(x = fecha, y = magnitud_cambio_promedio) ,na.rm=TRUE) +
  scale_x_date(date_breaks = "20 days", date_labels = "%Y %m %d") +
  theme(axis.text  = element_text(angle = 90)) + geom_hline(yintercept = cutoff, col= "red") + 
  labs(x = "dias", y = "Porcentaje") +
  annotate("text", x = xdate, y = 20, label = 20.3, color = "red")

