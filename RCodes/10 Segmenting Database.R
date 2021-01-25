
# Proceso de filtrado de la base de datos --------------------------------


#El objetivo de este script es poder filtrar aquellos productos que tienen
#pocas apariciones durante los dos años y medio del estudio o del proceso de 
#scrapeo. Se tiene 4 bases finales de las cuales solo se trabajará con una
#sola. Este es un segundo proceso de filtrado.


# (1) Limpieza de memoria, librerias --------------------------------------

rm(list =ls())
library(tidyverse)

#-------------------------
#(A) 01_Base_filtrada
#-------------------------
#Base eliminando los tamaños de cambio muy grandes
load("../output/MasterData/Base_Final_Filtrada.RData")
fname = "../output/DatosFiltrados/01_Base_filtrada.RData"
save(BASEDATOS,file=fname)


#-------------------------
#(B) 02_Base_filtrada_240
#-------------------------
#Esta es la base con la que se trabaja.
observaciones = BASEDATOS %>% count(ID) %>% filter(n>240)
BASEDATOS = BASEDATOS %>% filter(ID %in% observaciones$ID)

fname = "../output/DatosFiltrados/02_Base_filtrada_240.RData"
save(BASEDATOS,file=fname)

#-------------------------
#(C) 03_Base_filtrada_2017_2019
#-------------------------
load("../output/DatosFiltrados/01_Base_filtrada.RData")
B2017 = BASEDATOS %>% filter(fecha > "2017-03-31")
BASEDATOS = B2017

fname = "../output/DatosFiltrados/03_Base_filtrada_2017_2019.RData"
save(BASEDATOS,file=fname)

#-------------------------
#(D) 04_Base_filtrada_2019_270
#-------------------------

observaciones = B2017 %>% count(ID) %>% filter(n>270)
BASEDATOS = B2017 %>% filter(ID %in% observaciones$ID)

fname = "../output/DatosFiltrados/04_Base_filtrada_2019_270.RData"
save(BASEDATOS,file=fname)

#DONE!!