############################################
"Numero de cambios por dia PRECIOS INTERNET"
############################################
"Directorio"

rm(list=ls())
graphics.off() #Elimina cualquier grafico
directorio = dirname(rstudioapi::getSourceEditorContext()$path)
setwd(directorio)
dirmother = dirname(getwd())

#Librerias a utilizar

options(warn=0)
library(tidyverse)
library(readxl)
library(dplyr)
library(stringr)
library(ggplot2)
library(lubridate)
options(warn=2)

#-------------------------------------------------------------------
#Cargamos base de datos a utilizar(Puede ser cualquiera de las cuatro bases)
#-------------------------------------------------------------------
load("../Base/Base_Final_Peru_completa_01.RData")

#----------------------------
#Objetivo :Tabla cambios dia
#----------------------------
BASEDATOS <- base_ll
rm(base_ll)

#VARIABLE EVENTO
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

#Contabilizar el numero de cambios ocurridos en un mismo dia para todo el periodo de estudio en una tabla
cambios_dia = BASEDATOS %>% group_by(fecha) %>% distinct(fecha)
cambios_dia$numero_cambios = NA
cambios_dia$numero_precios = NA

#Creamos el loop y el vector de fechas a recorrer
contador = 1 
avector <- cambios_dia[['fecha']]
n = length(avector)
for (ii in 1:n) { 
  BD1 = filter(BASEDATOS, fecha == avector[ii])
  cambios_dia$numero_cambios[contador] = sum(BD1$Evento_internet,na.rm = TRUE)
  cambios_dia$numero_precios[contador] = length(BD1$Evento_internet)
  contador = contador + 1 
  rm(BD1)
}


#Numero de cambios por observacion
cambios_dia$numero_precios = as.numeric(cambios_dia$numero_precios)
cambios_dia$cambios_obs = 100*(cambios_dia$numero_cambios/cambios_dia$numero_precios)
#Ordenamos
cambios_dia <- cambios_dia[order(cambios_dia$fecha),]
cambios_dia$cambios_obs[1]<- NA

#cambios_dia =cambios_dia[order(cambios_dia$fecha),] #Ordenamos

# seq.Date crea un dataframe con fechas completas (L-D) entre las fechas indicadas
all_days = data.frame(fecha = seq.Date(from = min(cambios_dia$fecha), to = max(cambios_dia$fecha), by = "day"))

dd_complete = left_join(all_days, cambios_dia, by = "fecha", check.names=FALSE) #Check.names "importante"
dd_complete$day <- weekdays(as.Date(dd_complete$fecha))
dd_complete = filter(dd_complete,!(day=="sábado" | day=="domingo"))

#-------------------------------------------------------------------
#Gráfico de frecuencia de cambio de precios por día, incluye NAs
#-------------------------------------------------------------------
xdate <- as.Date("2016-08-12")
ggplot(data=dd_complete)+
  geom_point(mapping=aes(x=fecha,y=cambios_obs),na.rm=TRUE) + 
  geom_line(mapping=aes(x=fecha,y=cambios_obs),na.rm=TRUE)+
  scale_x_date(date_labels="%Y %m %d",date_breaks  ="25 days")+
  theme(axis.text.x = element_text(angle = 90, vjust=0.5),
        panel.grid.minor = element_blank()) +
  geom_hline(yintercept = mean(dd_complete$cambios_obs,na.rm = TRUE),
             color ="red") + 
  annotate("text", x=xdate, y=6.00, label="4.22", color ="blue") +
  labs(x = "dias", y = "Porcentaje")
  

#-------------------------------------------------------------------
#Gráfico de frecuencia de cambio de precios por día, sin NAs
#-------------------------------------------------------------------

ggplot(data=cambios_dia)+
  geom_point(mapping=aes(x=fecha,y=cambios_obs)) + 
  geom_line(mapping=aes(x=fecha,y=cambios_obs))+
  scale_x_date(date_labels="%b %y",date_breaks  ="1 month")

summary(cambios_dia)

#-------------------------------------------------------------------
#Gráfico de frecuencia de cambio de precios por mes
#-------------------------------------------------------------------

#Ver cambios promedios por mes
"mes"
cambios_dia$fechad = str_sub(cambios_dia$fecha,start=6,end=7)
"año"
cambios_dia$fechaa = str_sub(cambios_dia$fecha,start=1,end=4) 

# Promedio de cambios mensuales
mes_promedio = cambios_dia %>% group_by(fechaa,fechad) %>%
  summarise(mean = mean(cambios_obs, na.rm = TRUE))
mes_promedio = mes_promedio %>% mutate(mes = as.Date(paste0(fechaa,"-",fechad,"-","01")))

ggplot(mes_promedio,aes(x=mes, y=mean))+ geom_bar(stat = "identity")+
  scale_x_date(date_labels="%b %y",date_breaks  ="1 month")

# Mediana de cambios mensuales
mes_mediana = cambios_dia %>% group_by(fechaa,fechad) %>%
  summarise(median = median(cambios_obs, na.rm = TRUE))
mes_mediana = mes_mediana %>% mutate(mes = as.Date(paste0(fechaa,"-",fechad,"-","01")))

ggplot(mes_mediana,aes(x=mes, y=median))+ geom_bar(stat = "identity")+
  scale_x_date(date_labels="%b %y",date_breaks  ="1 month")

#DONE!!! :-P
#-----------------------------------------------------------------------