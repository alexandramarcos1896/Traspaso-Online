#Identificador dias vacios
#**************************

"Directorio"

rm(list=ls())
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
load(paste0(dirmother,"/3.Analisis de datos/P1_Output/01_base_original_cambiointernet.RData"))
names(BD.1)[13] <- "ID"

#Se crea una tabla donde aparescan por primera y ultima vez los ID's
inicio = BD.1 %>% group_by(descripcion, marca_descrip_id) %>% top_n(1, desc(fecha)) %>% select(fecha,descripcion, marca_descrip_id)
final = BD.1 %>% group_by(descripcion, marca_descrip_id) %>% top_n(-1, desc(fecha)) %>% select(fecha,descripcion, marca_descrip_id)
Tabla_dias = cbind(inicio, final) %>% select(marca_descrip_id,descripcion, fecha, fecha1)
rm(inicio,final)


#Identificado de numero de dias vs apariciones reales
Tabla_dias$numero_apariciones = NA  #Se contabiliza el numero de veces que aparece en la base
Tabla_dias$numero_dias = (Tabla_dias$fecha1 -Tabla_dias$fecha)+1 #Es el numero de dias desde su primera aparicion
#Hasta la ultima

contador = 1
id = BD.1 %>% distinct(marca_descrip_id)
avector <- id[["marca_descrip_id"]]
for (ii in avector) {
  Tabla_dias$numero_apariciones[contador] = nrow(filter(BD.1, marca_descrip_id == ii))
  contador = contador + 1
}

#Revisamos el type de las variables
sapply(Tabla_dias, class)
Tabla_dias$numero_dias = as.numeric(Tabla_dias$numero_dias)

#Creamos un indicador entre las dos variables creadas
Tabla_dias$indicador = round(Tabla_dias$numero_apariciones/Tabla_dias$numero_dias,3)
Tabla_dias$falta = Tabla_dias$numero_dias - Tabla_dias$numero_apariciones

qplot(falta, data = Tabla_dias, geom = "histogram",binwidth =8) # Se muestra subgrupos

"Guardamos"
sf = paste0(dirmother,"/Pass-through/Analisis_base/Indicador_dias_vacios.RData")
save(Tabla_dias, file = sf)
