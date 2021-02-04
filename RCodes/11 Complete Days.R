
# Completar dias ----------------------------------------------------------

# (1)Limpiar memoria y cargar librerias -----------------------------------
graphics.off()
rm(list=ls())
options(warn=0)
library(tidyverse)
library(dplyr)
library(data.table)
library(stringr)
options(warn=2)


# (2) Base de Datos -------------------------------------------------------

load("../output/DatosFiltrados/02_Base_filtrada_240.RData")
# base = BD.1 %>% select(c(1:7,13,8:10,12,14:15))%>% filter(ID %in% pl$ID) 
# rm(BD.1,pl)


# Parte 1 -----------------------------------------------------------------

#Completamos los dias vacios
source("./funciones/completardias.R")
base_c = completardias(BASEDATOS)

#Eliminamos sabados y domingos
base_c$day <- weekdays(as.Date(base_c$fecha))
base_C <- base_c
base_C <- subset(base_C, day != "domingo")
base_C <- subset(base_C, day != "sábado")

base_c <- base_C[-c(16:20)]

# Guardamos base
fn = "../output/MasterData/Base_Final_Peru_alterada_01.RData" 
save(base_c, file = fn)

rm(base_C, BASEDATOS)


# Parte 2 -----------------------------------------------------------------

#Completamos precios
source("./funciones/llenarnas.R")
source("./funciones/generalizar.R")

base_c$precio_internet_completo = base_c$precio_internet
base_ll = generalizar(base_c)

"Guardamos"
fv_save = "../output/MasterData/Base_Final_Peru_completa_01.RData"
save(base_ll,file = fv_save)

rm(base_c)

# Parte 3 -----------------------------------------------------------------

#Creamos la tabla de duraciones

source("./funciones/EncuentraDuraciones.R")
source("./funciones/generalizar2.R")

Nweekdays <- Vectorize(function(a, b) 
  sum(!weekdays(seq(a, b, "days")) %in% c("domingo", "sábado"))) #Funcion para el loop

tabla_duraciones <- generalizar2(base_ll)
#tabla_duraciones <- tabla_duraciones %>% filter(!is.na(DDias))
row.names(tabla_duraciones) <- c(1:nrow(tabla_duraciones))  

tabla_duraciones %>% distinct(id) #711 items donde cada uno tiene....
tabla_duraciones %>% group_by(id) %>% tally() #...Numero de duraciones por item

nn = base_ll %>% group_by(categoria, subcategoria,descripcion,ID) %>%
  distinct(categoria, subcategoria,descripcion,ID)
names(nn)[4]<- "id"

tabla_duraciones_total <- right_join(tabla_duraciones,nn)
tabla_duraciones <- tabla_duraciones_total %>% select(c(6,8,9,10,1:5,7))
#Cambiamos el formato de las fechas
tabla_duraciones$Dini <- format(as.Date(as.character(tabla_duraciones$Dini), format = "%Y%m%d" ),"%Y-%m-%d")
tabla_duraciones$Dfin <- format(as.Date(as.character(tabla_duraciones$Dfin), format = "%Y%m%d" ),"%Y-%m-%d")

tabla_duraciones$Dfin <- as.Date(tabla_duraciones$Dfin)
tabla_duraciones$Dini <- as.Date(tabla_duraciones$Dini)

#format(as.Date(as.character(20091231), format="%Y%m%d"),"%d-%m-%Y") EJEMPLO


# (3) Incorporacion de dummies de fechas de oferta: -----------------------

dias_oferta <- as.Date(c("2016-07-15","2016-07-16","2016-07-17",
                         "2016-11-28","2016-11-29","2016-09-24",
                         "2017-07-15","2017-07-16","2017-07-17",
                         "2017-09-30","2017-11-23","2018-07-09",
                         "2018-07-10","2018-07-11","2018-11-26",
                         "2018-11-27","2018-11-23","2018-09-28",
                         "2018-09-29","2019-04-08","2019-04-09",
                         "2019-04-10"))

tabla_duraciones$dias_oferta <- ifelse(tabla_duraciones$Dini %in% dias_oferta,1,0)


# (4) Guardamos base de datos ---------------------------------------------

fv_save = "../output/MasterData/Base_duraciones_01.RData"
save(tabla_duraciones,file = fv_save)
