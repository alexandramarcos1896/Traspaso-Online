#------------------------------
#Graficos de referencia
#------------------------------

#(1)Limpiar memoria y cargar librerias
#----------------------------------------
rm(list=ls())
options(warn=0)
library(tidyverse)
library(dplyr)
library(data.table)
library(stringr)
library(readxl)
options(warn=2)

#(2) Directorio de trabajo
#----------------------------------------
directorio = dirname(rstudioapi::getSourceEditorContext()$path)
setwd(directorio)
dirmother = dirname(getwd())

#(3) Base de Datos y partidas
#-----------------------------------------
load("../Base/Base_Final_Peru_completa_01.RData")
base_ll$ID <- as.factor(base_ll$ID)
partidas_lista <- read_excel(path = "../SagaFrecCuadro.xlsx", sheet = "Sheet1Modificado")


#(4) Bases
#-----------------------------

microondas_sf <- base_ll[grep("microondas",base_ll$subcategoria),]
cocinas_sf <- base_ll[grep("cocina",base_ll$subcategoria),]

tv1 <- grep("tv",base_ll$subcategoria)
rack <- grep("accesorios",base_ll$subcategoria)
tv <- setdiff(tv1,rack)
tv_sf <- base_ll[tv,]

laptop_sf <- base_ll %>% filter(subcategoria == "laptops")
refri_sf <- base_ll %>% filter(subcategoria=="refrigeración")

#(5) Filtar
#-----------------------------------------
#De acuerdo al arancel de ADUANAS 2017, los dos primeros digitos muestran la clasificación
#general del sistema armonizado. En base a esta clasificación se pretende reducir
#la lista anterior a solo los productos que puede categorizarse como electrónicos.

clasificacion <- c(73,84,85,90,91,92)

partidas_lista$capitulo <- substr(partidas_lista$partida,1,2)
partidas_lista$capitulo <- as.numeric(partidas_lista$capitulo)
partidas_lista_electro <- subset(partidas_lista,capitulo %in% clasificacion)


#(6) Proceso de emparejamiento
#-----------------------------------------
#(A)
#----
microondas_sf$partida <- partidas_lista_electro$partida[grep("microondas",partidas_lista_electro$producto)]
microondas_sf$MesReferencia <- substr(microondas_sf$fecha,1,7)
microondas_sf$MesReferencia <- gsub("-","",microondas_sf$MesReferencia)

#(B)
#----
cocinas_sf$partida <- 7321111200 #No se esta utilizando el excel reformado.
cocinas_sf$MesReferencia <- substr(cocinas_sf$fecha,1,7)
cocinas_sf$MesReferencia <- gsub("-","",cocinas_sf$MesReferencia)

#Reforma de la base de datos.
partidas_lista_reshaped <- melt(partidas_lista_electro,
                                id =c("partida","producto","pais_orige","pais",
                                      "capitulo"),
                                variable.name = "MesReferencia",
                                value.name = "Frecuencia")

#(7) Matching
#-----------------------------------------
microondas_sf <- merge(microondas_sf,partidas_lista_reshaped,
                       by = c("partida","MesReferencia"), sort = FALSE)

cocinas_sf <- merge(cocinas_sf,partidas_lista_reshaped,
                       by = c("partida","MesReferencia"), sort = FALSE)

#Ordenamos el merge
microondas_sf <- with(microondas_sf, microondas_sf[order(ID, fecha),])
cocinas_sf <- with(cocinas_sf, cocinas_sf[order(ID, fecha),])

#Identificacion de fechas. 
#--------------------------------

#Microondas
fechas_import <- partidas_lista_reshaped %>% 
  filter(partida == 8516500000) %>% filter(!is.na(Frecuencia))
#Cocinas. Existen dos posibles partidas
fechas_import_c1 <- partidas_lista_reshaped %>% 
  filter(partida == 7321111200) %>% filter(!is.na(Frecuencia))
fechas_import_c2 <- partidas_lista_reshaped %>% 
  filter(partida == 8516291000) %>% filter(!is.na(Frecuencia))
#Televisores
fechas_import_tv <- partidas_lista_reshaped %>% 
  filter(partida == 8528720000) %>% filter(!is.na(Frecuencia))
#laptop
fechas_import_lap <- partidas_lista_reshaped %>% 
  filter(partida == 8471300000) %>% filter(!is.na(Frecuencia))
#Refrigeradora
fechas_import_refri <- partidas_lista_reshaped %>% 
  filter(partida == 8414802200) %>% filter(!is.na(Frecuencia))


#Asumiendo que la importación se realiza 01
#---------------------------------------------
fechas_import$dias01 <- as.Date(paste0(
                        substr(fechas_import$MesReferencia,1,4),"-",
                        substr(fechas_import$MesReferencia,5,6),"-",
                        "01"))

fechas_import_c1$dias01 <- as.Date(paste0(
                        substr(fechas_import_c1$MesReferencia,1,4),"-",
                        substr(fechas_import_c1$MesReferencia,5,6),"-",
                        "01"))

fechas_import_c2$dias01 <- as.Date(paste0(
                        substr(fechas_import_c2$MesReferencia,1,4),"-",
                        substr(fechas_import_c2$MesReferencia,5,6),"-",
                        "01"))


fechas_import_tv$dias01<- as.Date(paste0(
                          substr(fechas_import_tv$MesReferencia,1,4),"-",
                          substr(fechas_import_tv$MesReferencia,5,6),"-",
                          "01"))

fechas_import_lap$dias01 <- as.Date(paste0(
                          substr(fechas_import_lap$MesReferencia,1,4),"-",
                          substr(fechas_import_lap$MesReferencia,5,6),"-",
                          "01"))

fechas_import_refri$dias01 <- as.Date(paste0(
                          substr(fechas_import_refri$MesReferencia,1,4),"-",
                          substr(fechas_import_refri$MesReferencia,5,6),"-",
                          "01"))
#Asumiendo que la importación se realiza 15
#---------------------------------------------
fechas_import$dias15 <- as.Date(paste0(
                        substr(fechas_import$MesReferencia,1,4),"-",
                        substr(fechas_import$MesReferencia,5,6),"-",
                        "15"))

fechas_import_c1$dias15 <- as.Date(paste0(
                        substr(fechas_import_c1$MesReferencia,1,4),"-",
                        substr(fechas_import_c1$MesReferencia,5,6),"-",
                        "15"))

fechas_import_c2$dias15 <- as.Date(paste0(
                        substr(fechas_import_c2$MesReferencia,1,4),"-",
                        substr(fechas_import_c2$MesReferencia,5,6),"-",
                        "15"))

fechas_import_tv$dias15 <- as.Date(paste0(
                          substr(fechas_import_tv$MesReferencia,1,4),"-",
                          substr(fechas_import_tv$MesReferencia,5,6),"-",
                          "15"))

fechas_import_lap$dias15 <- as.Date(paste0(
                          substr(fechas_import_lap$MesReferencia,1,4),"-",
                          substr(fechas_import_lap$MesReferencia,5,6),"-",
                          "15"))

fechas_import_refri$dias15 <- as.Date(paste0(
                          substr(fechas_import_refri$MesReferencia,1,4),"-",
                          substr(fechas_import_refri$MesReferencia,5,6),"-",
                          "15"))
#(8) Graficamos
#------------------------------------------
ID_micro <- microondas_sf %>% filter(fecha == min(fecha)) %>% 
            group_by(ID) %>% distinct(ID)

ID_cocina <- cocinas_sf %>% filter(fecha == min(fecha)) %>% 
  group_by(ID) %>% distinct(ID)
ID_cocina <- ID_cocina[1:10,]

ID_tv <- tv_sf %>% group_by(ID) %>% distinct(ID)

ID_refri <- refri_sf %>% filter(fecha == min(fecha))%>%
            group_by(ID) %>% distinct(ID)

#Se grafica todos las laptops

theme_set(theme_bw())

#---------------------------------------
# Asumiendo fecha de importacion 01 del mes
#---------------------------------------

#MiCROONDAS
#---------------------------------

#Grafico con precio original
#---------------
ggplot(data = microondas_sf %>% subset(ID %in% ID_micro$ID)) +
  geom_line(mapping = aes(x= fecha, y = precio_internet,
                          colour = ID),size = 1.0) +
  labs(title = "Microondas",
       subtitle = "Inicio del mes",
       x = NULL, y = NULL)+
  theme(axis.text.x = element_text(angle = 90, vjust=0.5),
        legend.position = "bottom",
        legend.box = "vertical") +
  geom_vline(xintercept = as.numeric(fechas_import$dias01),
             size = fechas_import$Frecuencia, colour = "firebrick") +
  scale_x_date(date_labels="%d %b %y",date_breaks  ="15 days")

sname <- "../Graficos_/Microondas_01_02.png"
ggsave(sname,width=12,height=7)
  

#Grafico con precio completo
#---------------
ggplot(data = microondas_sf %>% subset(ID %in% ID_micro$ID)) +
  geom_line(mapping = aes(x= fecha, y = precio_internet_completo,
                          colour = ID),size = 1.0) +
  labs(title = "Microondas",
       subtitle = "Inicio del mes",
       x = NULL, y = NULL)+
  theme(axis.text.x = element_text(angle = 90, vjust=0.5),
        legend.position = "bottom",
        legend.box = "vertical") +
  geom_vline(xintercept = as.numeric(fechas_import$dias01),
             size = fechas_import$Frecuencia, colour = "firebrick") +
  scale_x_date(date_labels="%d %b %y",date_breaks  ="15 days")

sname <- "../Graficos_/Microondas_01_01.png"
ggsave(sname,width=12,height=7)

#COCINAS
#---------------------------------

#Grafico con precio original
#---------------
ggplot(data = cocinas_sf %>% subset(ID %in% ID_cocina$ID)) +
  geom_line(mapping = aes(x= fecha, y = precio_internet,
                          colour = ID),size = 1.0) +
  labs(title = "Cocinas",
       subtitle = "Inicio del mes",
       x = NULL, y = NULL)+
  theme(axis.text.x = element_text(angle = 90, vjust=0.5),
        legend.position = "bottom",
        legend.box = "vertical") +
  geom_vline(xintercept = as.numeric(fechas_import_c1$dias01),
             size = fechas_import_c1$Frecuencia, colour = "firebrick") +
  geom_vline(xintercept = as.numeric(fechas_import_c2$dias01),
             size = fechas_import_c2$Frecuencia, colour = "darkblue")+
  scale_x_date(date_labels="%d %b %y",date_breaks  ="15 days")

sname <- "../Graficos_/Cocinas_01_02.png"
ggsave(sname,width=12,height=7)


#Grafico con precio completo
#---------------
ggplot(data = cocinas_sf %>% subset(ID %in% ID_cocina$ID)) +
  geom_line(mapping = aes(x= fecha, y = precio_internet_completo,
                          colour = ID),size = 1.0) +
  labs(title = "Cocinas",
       subtitle = "Inicio del mes",
       x = NULL, y = NULL)+
  theme(axis.text.x = element_text(angle = 90, vjust=0.5),
        legend.position = "bottom",
        legend.box = "vertical") +
  geom_vline(xintercept = as.numeric(fechas_import_c1$dias01),
             size = fechas_import_c1$Frecuencia, colour = "firebrick") +
  geom_vline(xintercept = as.numeric(fechas_import_c2$dias01),
             size = fechas_import_c2$Frecuencia, colour = "darkblue") +
  scale_x_date(date_labels="%d %b %y",date_breaks  ="15 days")

sname <- "../Graficos_/Cocinas_01_01.png"
ggsave(sname,width=12,height=7)


#TELEVISORES
#---------------------------------

#Grafico con precio original
#---------------
ggplot(data = tv_sf %>% subset(ID %in% ID_tv$ID)) +
  geom_line(mapping = aes(x= fecha, y = precio_internet,
                          colour = ID),size = 1.0) +
  labs(title = "Televisores",
       subtitle = "Inicio del mes",
       x = NULL, y = NULL)+
  theme(axis.text.x = element_text(angle = 90, vjust=0.5),
        legend.position = "bottom",
        legend.box = "vertical") +
  geom_vline(xintercept = as.numeric(fechas_import_tv$dias01),
             size = fechas_import_tv$Frecuencia, colour = "firebrick",
             alpha = 0.6) +
  scale_x_date(date_labels="%d %b %y",date_breaks  ="15 days")

sname <- "../Graficos_/Televisores_01_02.png"
ggsave(sname,width=12,height=7)


#Grafico con precio completo
#---------------
ggplot(data = tv_sf %>% subset(ID %in% ID_tv$ID)) +
  geom_line(mapping = aes(x= fecha, y = precio_internet_completo,
                          colour = ID),size = 1.0) +
  labs(title = "Televisores",
       subtitle = "Inicio del mes",
       x = NULL, y = NULL)+
  theme(axis.text.x = element_text(angle = 90, vjust=0.5),
        legend.position = "bottom",
        legend.box = "vertical") +
  geom_vline(xintercept = as.numeric(fechas_import_tv$dias01),
             size = fechas_import_tv$Frecuencia, colour = "firebrick",
             alpha = 0.6) +
  scale_x_date(date_labels="%d %b %y",date_breaks  ="15 days")

sname <- "../Graficos_/Televisores_01_01.png"
ggsave(sname,width=12,height=7)

#LAPTOPS
#----------------------------

#Grafico con precio original
#---------------
ggplot(data = laptop_sf) +
  geom_line(mapping = aes(x= fecha, y = precio_internet,
                          colour = ID),size = 1.0) +
  labs(title = "Laptops",
       subtitle = "Inicio del mes",
       x = NULL, y = NULL)+
  theme(axis.text.x = element_text(angle = 90, vjust=0.5),
        legend.position = "bottom",
        legend.box = "vertical") +
  geom_vline(xintercept = as.numeric(fechas_import_lap$dias01),
             colour = "firebrick",
             alpha = 0.6) +
  scale_x_date(date_labels="%d %b %y",date_breaks  ="15 days")

sname <- "../Graficos_/Laptops_01_02.png"
ggsave(sname,width=12,height=7)


#Grafico con precio completo
#---------------
ggplot(data = laptop_sf) +
  geom_line(mapping = aes(x= fecha, y = precio_internet_completo,
                          colour = ID),size = 1.0) +
  labs(title = "Laptops",
       subtitle = "Inicio del mes",
       x = NULL, y = NULL)+
  theme(axis.text.x = element_text(angle = 90, vjust=0.5),
        legend.position = "bottom",
        legend.box = "vertical") +
  geom_vline(xintercept = as.numeric(fechas_import_lap$dias01),
             colour = "firebrick") +
  scale_x_date(date_labels="%d %b %y",date_breaks  ="15 days")

sname <- "../Graficos_/Laptops_01_01.png"
ggsave(sname,width=12,height=7)

#Refrigeradora
#---------------------------------

#Grafico con precio original
#---------------
ggplot(data = refri_sf %>% subset(ID %in% ID_refri$ID)) +
  geom_line(mapping = aes(x= fecha, y = precio_internet,
                          colour = ID),size = 1.0) +
  labs(title = "Refrigeradoras y congeladores",
       subtitle = "Inicio del mes",
       x = NULL, y = NULL)+
  theme(axis.text.x = element_text(angle = 90, vjust=0.5),
        legend.position = "bottom",
        legend.box = "vertical") +
  geom_vline(xintercept = as.numeric(fechas_import_refri$dias01),
             size = fechas_import_refri$Frecuencia, colour = "firebrick",
             alpha = 0.6) +
  scale_x_date(date_labels="%d %b %y",date_breaks  ="15 days")

sname <- "../Graficos_/Refrigeradoras_01_02.png"
ggsave(sname,width=12,height=7)


#Grafico con precio completo
#---------------
ggplot(data = refri_sf %>% subset(ID %in% ID_refri$ID)) +
  geom_line(mapping = aes(x= fecha, y = precio_internet_completo,
                          colour = ID),size = 1.0) +
  labs(title = "Refrigeradoras",
       subtitle = "Inicio del mes",
       x = NULL, y = NULL)+
  theme(axis.text.x = element_text(angle = 90, vjust=0.5),
        legend.position = "bottom",
        legend.box = "vertical") +
  geom_vline(xintercept = as.numeric(fechas_import_refri$dias01),
             size = fechas_import_refri$Frecuencia, colour = "firebrick",
             alpha = 0.6) +
  scale_x_date(date_labels="%d %b %y",date_breaks  ="15 days")

sname <- "../Graficos_/Refrigeradora_01_01.png"
ggsave(sname,width=12,height=7)

#---------------------------------------
# Asumiendo fecha de importacion 15 del mes
#---------------------------------------

#MiCROONDAS
#---------------------------------

#Grafico con precio original
#---------------
ggplot(data = microondas_sf %>% subset(ID %in% ID_micro$ID)) +
  geom_line(mapping = aes(x= fecha, y = precio_internet,
                          colour = ID),size = 1.0) +
  labs(title = "Microondas",
       subtitle = "Quincena del mes",
       x = NULL, y = NULL)+
  theme(axis.text.x = element_text(angle = 90, vjust=0.5),
        legend.position = "bottom",
        legend.box = "vertical") +
  geom_vline(xintercept = as.numeric(fechas_import$dias15),
             size = fechas_import$Frecuencia, colour = "firebrick") +
  scale_x_date(date_labels="%d %b %y",date_breaks  ="15 days")

sname <- "../Graficos_/Microondas_15_02.png"
ggsave(sname,width=12,height=7)


#Grafico con precio completo
#---------------
ggplot(data = microondas_sf %>% subset(ID %in% ID_micro$ID)) +
  geom_line(mapping = aes(x= fecha, y = precio_internet_completo,
                          colour = ID),size = 1.0) +
  labs(title = "Microondas",
       subtitle = "Quincena del mes",
       x = NULL, y = NULL)+
  theme(axis.text.x = element_text(angle = 90, vjust=0.5),
        legend.position = "bottom",
        legend.box = "vertical") +
  geom_vline(xintercept = as.numeric(fechas_import$dias15),
             size = fechas_import$Frecuencia, colour = "firebrick") +
  scale_x_date(date_labels="%d %b %y",date_breaks  ="15 days")

sname <- "../Graficos_/Microondas_15_01.png"
ggsave(sname,width=12,height=7)

#COCINAS
#---------------------------------
#Grafico con precio original
#---------------
ggplot(data = cocinas_sf %>% subset(ID %in% ID_cocina$ID)) +
  geom_line(mapping = aes(x= fecha, y = precio_internet,
                          colour = ID),size = 1.0) +
  labs(title = "Cocinas",
       subtitle = "Quincena del mes",
       x = NULL, y = NULL)+
  theme(axis.text.x = element_text(angle = 90, vjust=0.5),
        legend.position = "bottom",
        legend.box = "vertical") +
  geom_vline(xintercept = as.numeric(fechas_import_c1$dias15),
             size = fechas_import_c1$Frecuencia, colour = "firebrick") +
  geom_vline(xintercept = as.numeric(fechas_import_c2$dias15),
             size = fechas_import_c2$Frecuencia, colour = "darkblue") +
  scale_x_date(date_labels="%d %b %y",date_breaks  ="15 days")

sname <- "../Graficos_/Cocinas_15_02.png"
ggsave(sname,width=12,height=7)


#Grafico con precio completo
#---------------
ggplot(data = cocinas_sf %>% subset(ID %in% ID_cocina$ID)) +
  geom_line(mapping = aes(x= fecha, y = precio_internet_completo,
                          colour = ID),size = 1.0) +
  labs(title = "Cocinas",
       subtitle = "Quincena del mes",
       x = NULL, y = NULL)+
  theme(axis.text.x = element_text(angle = 90, vjust=0.5),
        legend.position = "bottom",
        legend.box = "vertical") +
  geom_vline(xintercept = as.numeric(fechas_import_c1$dias15),
             size = fechas_import_c1$Frecuencia, colour = "firebrick") +
  geom_vline(xintercept = as.numeric(fechas_import_c2$dias15),
             size = fechas_import_c2$Frecuencia, colour = "darkblue") +
  scale_x_date(date_labels="%d %b %y",date_breaks  ="15 days")

sname <- "../Graficos_/Cocinas_15_01.png"
ggsave(sname,width=12,height=7)


#TELEVISORES
#---------------------------------

#Grafico con precio original
#---------------
ggplot(data = tv_sf %>% subset(ID %in% ID_tv$ID)) +
  geom_line(mapping = aes(x= fecha, y = precio_internet,
                          colour = ID),size = 1.0) +
  labs(title = "Televisores",
       subtitle = "Inicio del mes",
       x = NULL, y = NULL)+
  theme(axis.text.x = element_text(angle = 90, vjust=0.5),
        legend.position = "bottom",
        legend.box = "vertical") +
  geom_vline(xintercept = as.numeric(fechas_import_tv$dias15),
             size = fechas_import_tv$Frecuencia, colour = "firebrick",
             alpha = 0.6) +
  scale_x_date(date_labels="%d %b %y",date_breaks  ="15 days")

sname <- "../Graficos_/Televisores_15_02.png"
ggsave(sname,width=12,height=7)


#Grafico con precio completo
#---------------
ggplot(data = tv_sf %>% subset(ID %in% ID_tv$ID)) +
  geom_line(mapping = aes(x= fecha, y = precio_internet_completo,
                          colour = ID),size = 1.0) +
  labs(title = "Televisores",
       subtitle = "Inicio del mes",
       x = NULL, y = NULL)+
  theme(axis.text.x = element_text(angle = 90, vjust=0.5),
        legend.position = "bottom",
        legend.box = "vertical") +
  geom_vline(xintercept = as.numeric(fechas_import_tv$dias15),
             size = fechas_import_tv$Frecuencia, colour = "firebrick",
             alpha = 0.6) +
  scale_x_date(date_labels="%d %b %y",date_breaks  ="15 days")

sname <- "../Graficos_/Televisores_15_01.png"
ggsave(sname,width=12,height=7)

#LAPTOP
#---------------------------------

#Grafico con precio original
#---------------
ggplot(data = laptop_sf) +
  geom_line(mapping = aes(x= fecha, y = precio_internet,
                          colour = ID),size = 1.0) +
  labs(title = "Laptops",
       subtitle = "Inicio del mes",
       x = NULL, y = NULL)+
  theme(axis.text.x = element_text(angle = 90, vjust=0.5),
        legend.position = "bottom",
        legend.box = "vertical") +
  geom_vline(xintercept = as.numeric(fechas_import_lap$dias15),
             colour = "firebrick") +
  scale_x_date(date_labels="%d %b %y",date_breaks  ="15 days")

sname <- "../Graficos_/Laptops_15_02.png"
ggsave(sname,width=12,height=7)


#Grafico con precio completo
#---------------
ggplot(data = laptop_sf) +
  geom_line(mapping = aes(x= fecha, y = precio_internet_completo,
                          colour = ID),size = 1.0) +
  labs(title = "Laptops",
       subtitle = "Inicio del mes",
       x = NULL, y = NULL)+
  theme(axis.text.x = element_text(angle = 90, vjust=0.5),
        legend.position = "bottom",
        legend.box = "vertical") +
  geom_vline(xintercept = as.numeric(fechas_import_lap$dias15),
             colour = "firebrick") +
  scale_x_date(date_labels="%d %b %y",date_breaks  ="15 days")

sname <- "../Graficos_/Laptops_15_01.png"
ggsave(sname,width=12,height=7)

#Refrigeradora
#---------------------------------

#Grafico con precio original
#---------------
ggplot(data = refri_sf %>% subset(ID %in% ID_refri$ID)) +
  geom_line(mapping = aes(x= fecha, y = precio_internet,
                          colour = ID),size = 1.0) +
  labs(title = "Refrigeradoras y congeladores",
       subtitle = "Inicio del mes",
       x = NULL, y = NULL)+
  theme(axis.text.x = element_text(angle = 90, vjust=0.5),
        legend.position = "bottom",
        legend.box = "vertical") +
  geom_vline(xintercept = as.numeric(fechas_import_refri$dias15),
             size = fechas_import_refri$Frecuencia, colour = "firebrick",
             alpha = 0.6) +
  scale_x_date(date_labels="%d %b %y",date_breaks  ="15 days")

sname <- "../Graficos_/Refrigeradoras_15_02.png"
ggsave(sname,width=12,height=7)


#Grafico con precio completo
#---------------
ggplot(data = refri_sf %>% subset(ID %in% ID_refri$ID)) +
  geom_line(mapping = aes(x= fecha, y = precio_internet_completo,
                          colour = ID),size = 1.0) +
  labs(title = "Refrigeradoras",
       subtitle = "Inicio del mes",
       x = NULL, y = NULL)+
  theme(axis.text.x = element_text(angle = 90, vjust=0.5),
        legend.position = "bottom",
        legend.box = "vertical") +
  geom_vline(xintercept = as.numeric(fechas_import_refri$dias15),
             size = fechas_import_refri$Frecuencia, colour = "firebrick",
             alpha = 0.6) +
  scale_x_date(date_labels="%d %b %y",date_breaks  ="15 days")

sname <- "../Graficos_/Refrigeradora_15_01.png"
ggsave(sname,width=12,height=7)