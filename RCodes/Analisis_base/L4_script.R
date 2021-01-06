################
#SAGA FALABELLA#
################

rm(list=ls())

"Cargamos librerias"
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

"Cargamos base de datos"

#cambios = BASEDATOS %>% group_by(marca_descrip_id) %>% summarise(numero_cambios = sum(Evento2))

#cambios = merge(cambios, Estadisticos[c(1,11:14)])

"Creamos tabla con los estadisticos"
a = as.data.frame(BD.1$precio_member_completo) %>% filter(!is.na(BD.1$precio_member_completo))
colnames(a)[1]="precio"
a$log_price = log(a$precio)

b = as.data.frame(BD.1$precio_internet)
colnames(b)[1]="precio"
b$log_price = log(b$precio)

c = as.data.frame(BD.1$precio_normal_completo)
colnames(c)[1]="precio"
c$log_price = log(c$precio)

Precios = rbind.data.frame(a,b,c)

summary(Precios)
summary(a)
summary(b)
summary(c)

"Creamos tabla de estadisticos"
#Base total
text_tbl <- data.frame(
  Log_precios = c("Precios completos", "Precio Member", "Precio Internet", "Precio Normal"),
  Minimo = c("0.6419","2.634","0.6419","1.932"),
  Percentil_25 = c("4.9068", "5.477","4.7791","4.867"),
  Mediana = c("6.0379", "6.757","5.9475","5.989"),
  Promedio = c("6.0403", "6.434","5.9463","6.049"),
  Percentil_75 = c("7.2869","7.377","7.2071","7.313"),
  Maximo = c("12.6094", "9.798","12.6094","12.609")
  
)

#Base filtrada (n>240)

n = BD.1 %>% count(marca_descrip_id) %>% filter(n>240)
BD240 = BD.1 %>% filter(marca_descrip_id %in% n$marca_descrip_id)

"Creamos tabla con los estadisticos"
a = as.data.frame(BD240$precio_member_completo) %>% filter(!is.na(BD240$precio_member_completo))
colnames(a)[1]="precio"
a$log_price = log(a$precio)

b = as.data.frame(BD240$precio_internet)
colnames(b)[1]="precio"
b$log_price = log(b$precio)

c = as.data.frame(BD240$precio_normal_completo)
colnames(c)[1]="precio"
c$log_price = log(c$precio)

Precios = rbind.data.frame(a,b,c)

summary(Precios)
summary(a)
summary(b)
summary(c)

text_tbl2 <- data.frame(
  Log_precios = c("Precios completos", "Precio Member", "Precio Internet", "Precio Normal"),
  Minimo = c("1.589","3.196","1.589","1.932"),
  Percentil_25 = c("5.069", "5.293","5.004","5.069"),
  Mediana = c("5.989", "6.327","5.938","5.989"),
  Promedio = c("6.012", "6.164","5.936","6.036"),
  Percentil_75 = c("7.244","7.244","7.169","7.244"),
  Maximo = c("9.393", "8.987","9.393","9.393")
  
)

