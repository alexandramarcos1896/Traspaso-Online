#################
#Creacion del ID#
#################

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
#(2)Establecer directorio relativo
#--------------------------------

directorio = dirname(rstudioapi::getSourceEditorContext()$path)
setwd(directorio)
dirmother = dirname(directorio)

#--------------------------------
#(3)Base de datos
#--------------------------------

#Parte 1
load("../Base/PreBase(Actualizacion)/Base_Total_Peru.RData")
anterior=DF_TOTAL

#Parte2
fname = "../Base/PreBase(Actualizacion)/2018-09-14hasta2019-04-30.RData"
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
df_tot1$descripcion <- as.character(df_tot1$descripcion,width = NULL)
df_tot1$descripcion <- str_replace_all(df_tot1$descripcion,"\"","") 
df_tot1$descripcion <- str_replace_all(df_tot1$descripcion,"''","")
df_tot1$descripcion <- str_trim(df_tot1$descripcion, side= "both")
df_tot1$descripcion <- str_to_lower(df_tot1$descripcion)

#Marca
df_tot1$marca <- str_trim(df_tot1$marca, side= "both")

#Categoria
df_tot1$categoria <- str_trim(df_tot1$categoria, side= "both") # deletes spaces in the end and in the beginning of the string
df_tot1$categoria <- str_to_lower(df_tot1$categoria)

#Subcategoria
df_tot1$subcategoria <-str_to_lower(df_tot1$subcategoria)
df_tot1$subcategoria <-str_replace_all(df_tot1$subcategoria,"  "," ")
df_tot1$subcategoria <-str_trim(df_tot1$subcategoria, side= "both")


# modificar marcas con errores

df_tot1 = df_tot1 %>% mutate(marca = recode(marca,
                                      fuji="fujifilm",
                                      `kitchen aid`= "kitchenaid",
                                      `perfect choise` = "perfect choice", 
                                      practika = "práctika",
                                      proair = "pro air", 
                                      shenhua = "shen hua",
                                      skil = "skill",
                                      swissgear = "swiss gear")) 

#--------------------------------
#(5)Creacion de codigo unico
#--------------------------------

#Creamos identificador de cada producto en base a  su marca y descripción
df_tot1$id <- paste(df_tot1$marca, "_", df_tot1$descripcion)
df_tot1$marca_descrip_id <- df_tot1 %>% group_indices(marca,descripcion)
df_tot1$marca_descrip_id <- factor(df_tot1$marca_descrip_id)


#Crea panel ordenado por marca_descrip_id del producto y por fecha,
#Manteniendo la fecha en orden creciente, ordena el marca_descrip_id en orden ascendente
df_tot1 <- with(df_tot1, df_tot1[order(marca_descrip_id, fecha),])

#Eliminamos duplicados
final_db <-distinct(df_tot1) #2345889

#--------------------------------
#(6)Creacion de nuevas variables: Precios
#--------------------------------


# Precio normal completo
#-------------------------
#Donde no hay informacion del precio normal se completa con el precio de internet.
final_db$precio_normal_completo <- final_db$precio_normal
ww=is.na(final_db$precio_normal)
final_db$precio_normal_completo[ww]=final_db$precio_internet[ww]
final_db$precio_normal_completo=as.numeric(final_db$precio_normal_completo)

# Precio member completo
#-------------------------
#Se completa el precio member con la funcion fill.
final_db = final_db %>% mutate(precio_member_completo = precio_member) %>%
  group_by(marca_descrip_id) %>%
  fill(precio_member_completo)

#Eliminamos un error particular
#--------------------------------
final_db$precio_normal = str_replace_all(final_db$precio_normal,
                                         "299349","349")
final_db$precio_internet = str_replace_all(final_db$precio_internet,
                                           "299349","349")
final_db$precio_normal_completo = str_replace_all(final_db$precio_normal_completo,
                                                  "299349","349")
#--------------------------------
#(7)Formato de las variables
#--------------------------------
BD <- final_db

BD$marca <-as.factor(BD$marca)
BD$descripcion <- as.factor(BD$descripcion)
BD$id <- as.factor(BD$id)
BD$subcategoria <- as.factor(BD$subcategoria)
BD$rating <- as.numeric(BD$rating)
table(BD$rating=="NA")# si el numero que aparece en FALSE es menor al numero de filas de la base BD es porque existe NA
# o "", por ello debemos correr la sgte linea para que reemplace ello y ocurra error posteriormente
BD$rating[is.na(BD$rating)] <- as.numeric(0)

BD$precio_internet <- as.numeric(BD$precio_internet)
BD$precio_normal <- as.numeric(BD$precio_normal)
BD$precio_member_completo <- as.numeric(BD$precio_member_completo)
BD$precio_normal_completo <- as.numeric(BD$precio_normal_completo)

rm(df_tot1,final_db)

#Renombramos el identificador de los productos por ID.
names(BD)[13] <- "ID"

#--------------------------------
#(7)Guardamos bases de datos
#--------------------------------
filename=paste0("../Base/PreBase(Actualizacion)/Base_Final_Peru_1.RData")
save(BD, file=filename)

#DONE!!