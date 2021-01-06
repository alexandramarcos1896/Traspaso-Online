###########################
#TRASPASO- CARACTERISTICAS DEL BIEN
###########################

#El objetivo de este script es graficar la relacion del traspaso del tipo de cambio
#con algunas caracteristica del bien


#(1)Limpiar memoria y cargar librerias
#----------------------------------------
rm(list = ls())
librerias <- c("tidyverse","ggplot2",
               "stringr","dummies",
               "readxl","bizdays", "lmtest","dplyr")
lapply(librerias,library, character.only = TRUE)


library(gamlss)
library(evir)
library(gamlss.tr)
# Nweekdays <- Vectorize(function(a, b) 
#   sum(!weekdays(seq(a, b, "days")) %in% c("domingo", "sábado")))

#(2) Directorio de trabajo
#----------------------------------------
directorio = dirname(rstudioapi::getSourceEditorContext()$path)
setwd(directorio)
dirmother = dirname(directorio)


#(3) Base de Datos
#-----------------------------------------
load(paste0(dirmother,"/Base/Base_duraciones_01.RData")) #Base de duraciones
nexchange_rate <-read_excel(path = paste0(dirmother,"/USDPEN2.xlsx"),
                            "Sheet 1") #Tipo de cambio

#Se elimina aquellas duraciones de precios menores a 5 dias y los censurados.
tabla_duraciones <- filter(tabla_duraciones, DDias >=5)
tabla_duraciones <- filter(tabla_duraciones, Status==0)

sapply(nexchange_rate, class) #Revisamos los "types"
sapply(tabla_duraciones, class)

#Formato Date
nexchange_rate$fecha <- as.Date(nexchange_rate$fecha)

#Calendario para Perú
CalPeru <- create.calendar("CalPeru",
                           holidays = integer(0),
                           weekdays = c("saturday", "sunday"),
                           start.date = NULL,
                           end.date = NULL,
                           adjust.from = adjust.none,
                           adjust.to = adjust.none, financial = TRUE
)

#(4) Estimacion
#-----------------------------------------
#############################
#Modelo TAMAÑO DEL PRECIO
#############################
source("../regresion2.R")
#tabla_duraciones <- filter(tabla_duraciones,PInicial> 5000)
modelo6 <- regresion2(tabla_duraciones,nexchange_rate,20,0,2)
valores6 <- modelo6[[1]]$coefficients

table6 <- modelo6[[2]] %>% group_by(PInicial) %>%
  distinct(PInicial) %>% as.data.frame()
table6$PInicial <- sort(table6$PInicial)
table6$PT <- table6$PInicial*valores6[2,1]+valores6[1,1]
stargazer::stargazer(valores6)

mean(table6$PInicial)*valores6[2,1]+valores6[1,1]
#-------------------------------
#GRAFICO
theme_set(theme_bw())
ggplot() + geom_point(data = table6, mapping = aes(x=PInicial, y = PT),
                      color = "darkblue") +
  labs(y = "coeficiente de traspaso", x = "precio")+
  scale_y_continuous(breaks = seq(min(table6$PT),max(table6$PT), by = 0.30)) +
  scale_x_continuous(breaks = c(min(table6$PInicial),
                                table6$PInicial[seq(300,500, by =20)],
                                max(table6$PInicial)))+
  theme(axis.text.x = element_text(angle = 90, vjust=0.5), panel.grid.minor = element_blank())
  

#############################
#MODELO FRECUENCIA 
#############################
#Coeficiente de traspaso 20 dias atras. Periodo de importación
source("../regresion.R")
#-------------------------------
modelo16 <- regresion(tabla_duraciones,nexchange_rate,20,0,2)
valores16 <- modelo16[[1]]$coefficients

table16 <- modelo16[[2]] %>% group_by(demeanedfreq) %>%
  distinct(demeanedfreq) %>% as.data.frame()
table16$demeanedfreq <- sort(table16$demeanedfreq)
table16$PT <- table16$demeanedfreq*valores16[2,1]+valores16[1,1]

#traspaso promedio
AA <- modelo16[[2]]
mean(AA$demeanedfreq)*valores16[2,1]+valores16[1,1]

stargazer::stargazer(valores16)

ggplot() + geom_point(data = table16,
                      mapping = aes(x=demeanedfreq, y = PT), color = "darkblue") +
labs(y = "coeficiente de traspaso", x = "frecuencia de ajuste del bien")+
  scale_y_continuous(breaks = seq(min(table16$PT),c(max(table16$PT)+10),by = 0.30))+
  scale_x_continuous(breaks = seq(min(table16$demeanedfreq),
                                  max(table16$demeanedfreq)+10,by = 5))+
  theme(axis.text.x = element_text(angle = 90, vjust=0.5),
        panel.grid.minor = element_blank())

#############################
#MODELO CATEGORIAS
#############################
source("../regresion3.R")
modelo3 <- regresion3(tabla_duraciones,nexchange_rate,0,0,2)
valores3 <- modelo3[[1]]$coefficients
stargazer::stargazer(valores3)

#Audio
valores3[1,1]
#Computadores
valores3[1,1]+valores3[14,1]
#Electrodomesticos
valores3[1,1]+valores3[15,1]
#Electrohogar
valores3[1,1]+valores3[16,1]
#Fotografia
valores3[1,1]+valores3[17,1]
#Linea Blanca
valores3[1,1]+valores3[18,1]
#Telefonos
valores3[1,1]+valores3[19,1]
#Televisores
valores3[1,1]+valores3[20,1]


