#########
#Labor 2#
#########

"L2: Este script genera graficos de densidad e histogramas del n?mero de precios por cada tipo de precio de la base(normal, internet o member) asi como para cada categoria"

rm(list=ls())

#install.packages("expss")

"Cargamos librerias a utilizar"
options(warn=0)
library(tidyverse)
library(readxl)
library(dplyr)
library(stringr)
library(ggplot2)
library(qplot)
library(expss)
library(kableExtra)
options(warn=2)

"Directorio"
rm(list=ls())
directorio = dirname(rstudioapi::getSourceEditorContext()$path)
setwd(directorio)
dirmother = dirname(getwd())

"Tabla del proceso anterior"

load("./Estadisticos_precios_01.RData") #Tabla generada de L1 cuadro1

"Histograma y  densidad del numero de precios"

"Sin categorias"

#Agregado
qplot(PInt_count, data = Estadisticos , geom = "density", main="Grafico de densidad-Agregado", xlab = "Numero de precios por ID")
qplot(PInt_count, data = Estadisticos, geom = "histogram", binwidth =8, ylab ="Count"  ,xlab = "Numero de precios por ID")

#Por Precios especificos
"Numero de precio por ID por tipo de precios"
#Normal
qplot(PNormal_count, data = Estadisticos, geom = "density", main="Grafico de densidad-Precio Normal", xlab = "Numero de precios por ID", ylab ="Density")
qplot(PNormal_count, data = Estadisticos, geom = "histogram", main="Histograma-Precio Normal",binwidth =8, ylab ="Count"  ,xlab = "Numero de precios por ID")

#Internet
qplot(PInt_count, data = Estadisticos, geom = "density", main="Grafico de densidad-Precio Internet", xlab = "Numero de precios por ID", ylab ="Density")
qplot(PInt_count, data = Estadisticos, geom = "histogram", main="Histograma-Precio Internet", binwidth =8, ylab ="Count"  ,xlab = "Numero de precios por ID")

#Member
qplot(PMemb_count, data = Estadisticos, geom = "density", main="Grafico de densidad-Precio Member", xlab = "Numero de precios por ID", ylab ="Density")
qplot(PMemb_count, data = Estadisticos, geom = "histogram", main="Histograma-Precio Member", binwidth =8, ylab ="Count"  ,xlab = "Numero de precios por ID")

"Categorias"

#Normal 
qplot(PNormal_count, data = Estadisticos, geom = "density", main="Grafico de densidad-Precio Normal", xlab = "Numero de precios por ID", ylab ="Density", colour = categoria)
qplot(PNormal_count, data = Estadisticos, geom = "histogram", main="Histograma-Precio Normal",binwidth =8, ylab ="Count"  ,xlab = "Numero de precios por ID", fill = categoria)

#Internet
qplot(PInt_count, data = Estadisticos, geom = "density",
      xlab = "Número de precios por ID",
      ylab ="Densidad", colour = categoria)
qplot(PInt_count, data = Estadisticos, geom = "histogram", main="Histograma-Precio Internet", binwidth =5, ylab ="Count"  ,xlab = "Numero de precios por ID", fill = categoria)

#Member
qplot(PMemb_count, data = Estadisticos, geom = "density", main="Grafico de densidad-Precio Member", xlab = "Numero de precios por ID", ylab ="Density", colour = categoria)
qplot(PMemb_count, data = Estadisticos, geom = "histogram", main="Histograma-Precio Member", binwidth =10, ylab ="Count"  ,xlab = "Numero de precios por ID", fill = categoria)

#DONE!!!


#-----------------------------
#GRAFICOS PRESENTACION
#-----------------------------
theme_set(theme_bw())
ggplot(data = Estadisticos, aes(x = PInt_count))+
  geom_histogram(color="darkblue", fill="lightblue") +
  geom_vline(aes(xintercept=mean(PInt_count)),
               color="red", linetype="dashed", size=1) +
  labs(x = "Número de observaciones")

ggplot(data = Estadisticos, aes(x = PInt_count,
                                color = categoria,
                                fill = categoria))+
  geom_density(position="identity", alpha=0.2) +
  labs(x = "Número de observaciones", y = "Densidad") +
theme(legend.position="bottom")

