################################################
#Modificaciones al tipo de cambio
################################################

#(1)Limpiar memoria y cargar librerias
#----------------------------------------
graphics.off()
rm(list=ls())
options(warn=0)
library(tidyverse)
library(dplyr)
library(stringr)
library(readxl)
library(openxlsx)
options(warn=2)

#(2) Directorio de trabajo
#----------------------------------------
directorio = dirname(rstudioapi::getSourceEditorContext()$path)
setwd(directorio)
dirmother = dirname(getwd())

#(3) Base de Datos
#-----------------------------------------
#Tipo de cambio
nexchange_rate <-read_excel(path = paste0(dirmother,"/USDPEN.xlsx"),1)
sapply(nexchange_rate, class) #Revisamos los "types"

#Formato Date
nexchange_rate$fecha <- as.Date(nexchange_rate$fecha)

#Como no existe informacion del tipo de cambio en los dias feriados,sabados y
#domingos, se procede a crear una base con dias continuos y el proceso de
#llenado es con la funcion FILL.

all_days <- data.frame(fecha = seq.Date(from = min(nexchange_rate$fecha),
                                        to = max(nexchange_rate$fecha),
                                        by = "day"))

exchange_completed <- left_join(all_days, 
                         nexchange_rate,
                         by = "fecha",
                         check.names=FALSE)

exchange_completed <- exchange_completed %>% fill(USDPEN)
exchange_completed$dias <- weekdays(exchange_completed$fecha)
exchange_completed <- exchange_completed %>% 
                      filter(dias != "domingo" &
                       dias != "sábado")
exchange_completed <- exchange_completed[-3]

#Guardamos
fname = "../USDPEN2.xlsx"
write.xlsx(exchange_completed, file = fname)
