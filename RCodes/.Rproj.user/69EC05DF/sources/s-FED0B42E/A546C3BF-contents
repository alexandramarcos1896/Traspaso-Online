
# Correccion de categorias y subcategorias --------------------------------

#Este script se realiza la reduccion y uniformizacion de la clasificacion de los productos
#de la tienda SF, de tal manera que cuadren con la CLASIFICACION de la base de importaciones.

#(1)Limpiar memoria y cargar librerias
#----------------------------------------

rm(list = ls())
librerias <- c("tidyverse","dplyr","ggplot2","stringr", "tm", "data.table")
lapply(librerias, library, character.only =TRUE)


#(2) Base de Datos
#-----------------------------------------
load("./Base/MasterData/BasePERU_Proceso1.RData")
#(3) Eliminamos subcategorias : VER TODO...
#-----------------------------------------


#Tecnologia (desaparece seccion)
#--------------------------------
tecnologia <- subset(base2, categoria =="tecnología")
tecnologia <- droplevels(tecnologia)
#Se encuentra que las categoría tecnologia agrupa partes de otras categorias
table(tecnologia$subcategoria)
#Se procede a desintegrarla.

tecnologia1 <- grep("tecnología",BASEDATOS$categoria)

#audio
audio_tecno1 <- grep("audio",BASEDATOS$subcategoria)
audio_tecno <- intersect(audio_tecno1,tecnologia1)
BASEDATOS$categoria[audio_tecno]<-"audio"
#cámaras digitales
camaras_tecno1 <- grep("cámaras digitales",BASEDATOS$subcategoria)
camaras_tecno <- intersect(camaras_tecno1,tecnologia1)
BASEDATOS$categoria[camaras_tecno]<-"fotografía"
#computadoras y tablets
compu_tecno1 <- grep("computadoras y tablets",BASEDATOS$subcategoria)
compu_tecno <- intersect(compu_tecno1,tecnologia1)
BASEDATOS$categoria[compu_tecno]<-"computadores"
#telefonía
telefonia_tecno1 <- grep("telefonía",BASEDATOS$subcategoria)
telefonia_tecno <- intersect(telefonia_tecno1,tecnologia1)
BASEDATOS$categoria[telefonia_tecno]<-"teléfonos"
#televisores
televisores_tecno1 <- grep("televisores",BASEDATOS$subcategoria)
televisores_tecno2 <- grep("tv lcd",BASEDATOS$subcategoria)
tele <- sort(c(televisores_tecno1,televisores_tecno2))
televisores_tecno <- intersect(tele,tecnologia1)
BASEDATOS$categoria[televisores_tecno]<-"televisores"

#Linea Blanca
#-----------------------

#Electrodomésticos
#-----------------------
#electrodométicos <- subset(BASEDATOS, categoria =="electrodomésticos")
# electro2 <- subset(BASEDATOS1, categoria =="electrodomésticos")
# electrodométicos <- droplevels(electrodométicos)
# table(electrodométicos$subcategoria)

#hidrolavadora k1  1200w
#combo: secadora d/cabello 2000w + ondulador cable giratorio + alisador sg-3515
#purificador de agua lima pure elite

#Fotografia
#-----------------------




#(5)SUBCATEGORIA
#---------------------------

#Objetivo de este paso, que la subcategoria refleje la descripcion del producto.
#Es decir si la descripcion dice microondas, la subcategoria deberia decir
#microondas o una palabra que represente al bien.


#Microondas
#-----------
microondas1 <- grep("microondas",BASEDATOS$descripcion)
#Todos menos 
b1 <- grep("biberones",BASEDATOS$descripcion)
b2 <- grep("kit built trim",BASEDATOS$descripcion)
b3 <- grep("combo",BASEDATOS$descripcion)
b4 <- grep("refrigerador",BASEDATOS$descripcion)

microondas <- setdiff(microondas1, c(b1,b2,b3,b4))
BASEDATOS$subcategoria[microondas] <- "microondas"


#Cocinas
#----------
cocinas1 <- grep("cocina", BASEDATOS$descripcion)
BASEDATOS$subcategoria[cocinas1] <- "cocina" 


#(5) Reducir el numero de subcategorias y categorias
#-----------------------------------------
BASEDATOS1 <- CorrigeCategorias(BASEDATOS)
BASEDATOS <- CorrigeSubcategorias(BASEDATOS1)
BASEDATOS$subcategoria <- as.character(BASEDATOS$subcategoria)
BASEDATOS$subcategoria <- stripWhitespace(BASEDATOS$subcategoria)
BASEDATOS$descripcion <- as.character(BASEDATOS$descripcion)
BASEDATOS$descripcion <- stripWhitespace(BASEDATOS$descripcion)

#(6) Eliminamos duplicados
#------------------------------
BASEDATOS <- distinct(BASEDATOS)
BASEDATOS=BASEDATOS[!duplicated(BASEDATOS[c("fecha","ID")]),]

#(7) Guardamos base de datos
#------------------------------
filename=paste0("../Base/PreBase(Actualizacion)/Base_Final_Peru_2.RData")
save(BASEDATOS, file=filename)
