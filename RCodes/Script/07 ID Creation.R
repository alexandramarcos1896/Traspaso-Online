
# Creacion ID a nivel de producto -----------------------------------------

#En este script se llama la base de datos corregida y la base de datos actualizada.
#La finalidad es generar una base de datos gran en formato panel.


#--------------------------------
#(1)Limpiamos memoria y librerias
#--------------------------------

rm(list=ls())

options(warn=0)
library(tidyverse)
library(readxl)
library(readr)
library(scales)
library(dplyr)
library(stringr)
library(ggplot2)
options(warn=2)


#--------------------------------
#(3)Base de datos
#--------------------------------

#Parte 1
load("./Base/MasterData/Base_Total_Peru.RData")
anterior=DF_TOTAL

#Parte2
fname = "./Base/MasterData/2018-09-14hasta2019-04-30.RData"
load(fname)
actual = DATA

rm(DF_TOTAL,DATA)

#Unimos
df_tot1 = rbind(anterior,actual)
rm(actual,anterior)

#--------------------------------
#(4)Limpieza
#--------------------------------

#Marca
df_tot1$marca <- str_replace(df_tot1$marca, "ga.ma", "gama")

#Descripcion
df_tot1 <- df_tot1 %>% mutate(descripcion = as.character(descripcion,width = NULL),
                              descripcion = gsub("\\\\","",descripcion),
                              descripcion = gsub("'","",descripcion),
                              descripcion = str_trim(descripcion, side= "both"),
                              descripcion = str_to_lower(descripcion))
                        
#Marca
df_tot1$marca <- str_trim(df_tot1$marca, side= "both")

#Categoria
df_tot1 <- df_tot1 %>% mutate(categoria = str_trim(categoria, side= "both"),
                              categoria = str_to_lower(categoria)) # deletes spaces in the end and in the beginning of the string

#Subcategoria
df_tot1 <- df_tot1 %>% mutate(subcategoria = str_to_lower(subcategoria),
                              subcategoria = gsub("  "," ",subcategoria),
                              subcategoria = str_trim(subcategoria,side = "both"))

sort(unique(df_tot1$marca))

#Eliminamos un error particular
#--------------------------------
df_tot1$precio_normal = str_replace_all(df_tot1$precio_normal,
                                     "299349","349")
df_tot1$precio_internet = str_replace_all(df_tot1$precio_internet,
                                       "299349","349")
#--------------------------------
#(5)Creacion de codigo unico
#--------------------------------
df_tot1 <- df_tot1 %>% mutate(id = paste(marca,descripcion,sep = "_"))%>%
  left_join(df_tot1 %>% select(marca,descripcion)%>% unique()%>% 
                                   mutate(ID = 1:n()), by = c("marca","descripcion"))%>%
  arrange(ID,fecha) %>% distinct()

#--------------------------------
#(6)Creacion de nuevas variables: Precios
#--------------------------------

base <- df_tot1

# Precio normal completo
#-------------------------
#Donde no hay informacion del precio normal se completa con el precio de internet.

base <- base %>% mutate(precio_normal_completo = ifelse(is.na(precio_normal),precio_internet,precio_normal),
                        precio_normal_completo = as.numeric(precio_normal_completo))

# Precio member completo
#-------------------------
#Se completa el precio member con la funcion fill.
base = base %>% mutate(precio_member_completo = precio_member) %>%
  group_by(ID) %>%
  fill(precio_member_completo)


#--------------------------------
#(7)Formato de las variables
#--------------------------------

base <- base %>% mutate(marca = as.factor(marca), descripcion =as.factor(descripcion),
                        id = as.factor(id),subcategoria = as.factor(subcategoria),
                        rating = as.numeric(rating))

#--------------------------------
#(7)Guardamos bases de datos
#--------------------------------
filename=paste0("./Base/MasterData/BasePERU_Proceso1.RData")
save(base, file=filename)

#DONE!!
