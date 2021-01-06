#########
#Labor 1#
#########
"L1: Este script elabora dos tablas donde dependiendo de la base de datos ha trabajar elabora un cuadro de estadisticas (min,max,mean) de los precios
por cada producto y la siguiente tabla sintentiza la base por sus categorias, asi como el n?mero de observaciones por cada divisi?n"

rm(list=ls())

#install.packages("expss")

"Cargamos librerias a utilizar"
options(warn=0)
library(tidyverse)
library(readxl)
library(dplyr)
library(stringr)
library(ggplot2)
library(expss)
library(kableExtra)
options(digits = 3)
options(warn=2)


"Directorio"
rm(list=ls())
directorio = dirname(rstudioapi::getSourceEditorContext()$path)
setwd(directorio)
dirmother = dirname(getwd())

"Cargamos base de datos completa"
load(paste0(dirmother, "/Base/Base_Final_Peru_completa_01.RData")) #Insertar la base de datos a trabajar
#load(paste0(dirmother, "/Base/Base_filtradas/03_Base_filtrada_2017_2019.RData"))
BD = base_ll #Renombrarla para poder correr todo el script
BD$ID <- as.factor(BD$ID)
rm(base_ll)

BD <- droplevels(BD)
"Periodo de estudio"
min(BD$fecha)#Primer registro de un producto
max(BD$fecha)#Ultimo registro de un producto

"Numero de items en la base"
length(levels(unique(BD$ID))) #14301

"Numero de ves que cada item ha aparecido en el tiempo"
Num1 = count(BD, vars = ID)

"Estadisticos"
Summary <- data.frame(
  aggregate(precio_internet_completo~ID, data=BD, min),
  aggregate(precio_internet_completo~ID, data=BD, max),
  aggregate(precio_internet_completo~ID, data=BD, mean),
  aggregate(precio_normal_completo~ID, data=BD, min),
  aggregate(precio_normal_completo~ID, data=BD, max),
  aggregate(precio_normal_completo~ID, data=BD, mean)
)

Summary <- Summary[,c(1,2,4,6,8,10,12)]
colnames(Summary) <- c("ID", "internet_min", "internet_max", "internet_mean","normal_min","normal_max","normal_mean")


Summarya <- data.frame(
  aggregate(precio_member_completo~ID, data=BD, min),
  aggregate(precio_member_completo~ID, data=BD, max),
  aggregate(precio_member_completo~ID, data=BD, mean)
)

Summarya <- Summarya[,c(1,2,4,6)]
colnames(Summarya) <- c("ID", "member_min", "member_max", "member_mean")

#Merge /// Ambas tablas anteriores
Estadisticos = left_join(Summary,Summarya)

Estadisticos$PNormal_count = NA
Estadisticos$PMemb_count= NA
Estadisticos$PInt_count = NA

#Vector a utilizar en el loop siguente

ids <- Estadisticos[["ID"]]

ii = 1
for (ii in 1:nrow(Estadisticos)) {
  BD1 = filter(BD, ID == ids[ii])
  BD1 = droplevels(BD1)
  Estadisticos$PNormal_count[ii] = length(which(BD1$precio_normal_completo != "NA")) 
  Estadisticos$PMemb_count[ii] = length(which(BD1$precio_member_completo != "NA")) 
  Estadisticos$PInt_count[ii] = length(which(BD1$precio_internet_completo != "NA")) 
  ii = ii + 1
}

ID = BD %>% group_by(categoria,ID) %>% distinct(ID) 
Estadisticos = merge(Estadisticos,ID , by = "ID")
Estadisticos = Estadisticos[order(Estadisticos$ID),] #Agregamos categoria y ordenamos

#sv_name = paste0("H:/SGInvEco/ALMACEN_TRABAJO/Alexandra/Pass-through/Analisis_base/","Estadisticos_precios_01", ".RData")
sv_name = "./Estadisticos_precios_01.RData"
save(Estadisticos, file = sv_name)
#kable(Table2, "latex", booktabs = T)

#Tabla2 (Resume la base de datos segun sus categorías)
"Numero de observaciones por categoría"

cat = Estadisticos %>% group_by(categoria) %>% tally()
dat = Estadisticos %>% group_by(categoria) %>% summarise(observaciones = sum(PInt_count))

cat = merge(cat, dat, by ="categoria")
rm(dat,ID,Summary,Summarya)

cat$obs_item = cat$observaciones/cat$n

