#----------------------------------------
#Partidas arancelarias SF
#----------------------------------------

#Este script se realiza inspeccion de partidas arancelarias de los productos importados
#por SF.

#(1)Limpiar memoria y cargar librerias
#----------------------------------------

rm(list = ls())
options(warn = 0)
librerias <- c("tidyverse","dplyr","ggplot2","stringr", "tm","readxl","openxlsx")
lapply(librerias, library, character.only =TRUE)


#(2) Directorio de trabajo
#----------------------------------------

directorio = dirname(rstudioapi::getSourceEditorContext()$path)
setwd(directorio)
dirmother = dirname(directorio)

#(3) Base de Datos
#-----------------------------------------
partidas_totales <-read_excel(path = "../SagaFrecCuadro.xlsx",sheet = "Hoja1")
partidas_totales <- partidas_totales[,c(1,2)]
partidas_lista <- partidas_totales[!duplicated(partidas_totales[c(1,2)]),]


#(4) Filtar
#-----------------------------------------
#De acuerdo al arancel de ADUANAS 2017, los dos primeros digitos muestran la clasificación
#general del sistema armonizado. En base a esta clasificación se pretende reducir
#la lista anterior a solo los productos que puede categorizarse como electrónicos.

clasificacion <- c(73,84,85,90,91,92)

partidas_lista$capitulo <- substr(partidas_lista$partida,1,2)
partidas_lista$capitulo <- as.numeric(partidas_lista$capitulo)
partidas_lista_electro <- subset(partidas_lista,capitulo %in% clasificacion)

#(5) Completar nombres de las partidas
#-----------------------------------------
#Esto se logran en base a dos documentos : ADUANAS Y LISTA ARANCELARIA-PERU.
partidas_73 <- filter(partidas_lista_electro,capitulo==73)
partidas_84 <- filter(partidas_lista_electro,capitulo==84)
partidas_85 <- filter(partidas_lista_electro,capitulo==85)
partidas_90 <- filter(partidas_lista_electro,capitulo==90)
partidas_91 <- filter(partidas_lista_electro,capitulo==91)
partidas_92 <- filter(partidas_lista_electro,capitulo==92)

#(6) Guardamos formato xlsx
#-----------------------------------------
write.xlsx(partidas_73, file = "../Base_partidas/partida73.xlsx",row.names = FALSE)
write.xlsx(partidas_84, file = "../Base_partidas/partida84.xlsx",row.names = FALSE)
write.xlsx(partidas_85, file = "../Base_partidas/partida85.xlsx",row.names = FALSE)
write.xlsx(partidas_90, file = "../Base_partidas/partida90.xlsx",row.names = FALSE)
write.xlsx(partidas_91, file = "../Base_partidas/partida91.xlsx",row.names = FALSE)
write.xlsx(partidas_92, file = "../Base_partidas/partida92.xlsx",row.names = FALSE)
