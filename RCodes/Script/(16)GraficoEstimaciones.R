###########################
#Graficos Estimaciones
###########################

#(1)Limpiar memoria y cargar librerias
#----------------------------------------
rm(list = ls())
librerias <- c("tidyverse","ggplot2",
               "stringr","dummies",
               "readxl","bizdays")
lapply(librerias,library, character.only = TRUE)

#(2) Directorio de trabajo
#----------------------------------------
directorio = dirname(rstudioapi::getSourceEditorContext()$path)
setwd(directorio)
dirmother = dirname(directorio)

#(3) Base de Datos
#-----------------------------------------
load("../Base/matrix_regresiones.RData")

alpha_matrix <- as.data.frame(alpha_matrix)
alpha_matrix$j <- as.numeric(rownames(alpha_matrix))

alpha_matrix <- as.data.frame(alpha_matrix)

#(4) Graficos
#-----------------------------------------
ggplot(data = alpha_matrix) + 
  geom_point(aes(x = j, y =`0`, colour = "0")) +
  geom_line(mapping = aes(y=`5`, x= j, colour = "5")) + 
  geom_line(mapping = aes(y=`10`, x= j, colour = "10")) + 
  geom_line(mapping = aes(y=`20`, x= j, colour = "20")) +
  geom_line(mapping = aes(y=`25`, x= j, colour = "25")) +
  geom_line(mapping = aes(y=`30`, x= j, colour = "30")) +
  theme(legend.position = "bottom") + 
  scale_x_continuous(breaks = c(0:30))+
  labs(color ="Aumento de ventana en")

sname <- "../Graficos_/alpha1.png"
ggsave(sname,width=12,height=7)

std_matrix <- as.data.frame(std_matrix)
std_matrix$j <- as.numeric(rownames(std_matrix))

ggplot(data = alpha_matrix) + 
  geom_line(aes(x = j, y =`0`, colour = "0")) +
  geom_ribbon(aes(ymin =alpha_matrix$`0`-2*std_matrix$`0`,
                  ymax =alpha_matrix$`0`+2*std_matrix$`0`,
                  x =j,fill='firebrick3'),
              alpha = 0.2) +
  geom_line(mapping = aes(y=`10`, x= j, colour = "10")) + 
  geom_ribbon(aes(ymin =alpha_matrix$`10`-2*std_matrix$`10`,
                  ymax =alpha_matrix$`10`+2*std_matrix$`10`,
                  x =j,fill='darkblue'),
              alpha = 0.2) +
  scale_fill_identity() +
  labs(y = NULL) +
  guides(fill = guide_legend(show = FALSE))+
  theme(legend.position = "bottom") +
  scale_x_continuous(breaks = c(0:30)) +
  labs(color ="Aumento de ventana en")

sname <- "../Graficos_/alpha2.png"
ggsave(sname,width=12,height=7)

ggplot(data = alpha_matrix) + 
  geom_line(mapping = aes(y=`20`, x= j, colour = "20")) +
  geom_ribbon(aes(ymin =alpha_matrix$`20`-2*std_matrix$`20`,
                  ymax =alpha_matrix$`20`+2*std_matrix$`20`,
                  x =j,fill='red'),
              alpha = 0.2) +
  geom_line(mapping = aes(y=`30`, x= j, colour = "30")) +
  geom_ribbon(aes(ymin =alpha_matrix$`30`-2*std_matrix$`30`,
                  ymax =alpha_matrix$`30`+2*std_matrix$`30`,
                  x =j,fill='green'),
              alpha = 0.2) +
  scale_fill_identity() +
  labs(y = NULL) +
  scale_x_continuous(breaks = c(0:30))+
  guides(fill = guide_legend(show = FALSE))+
  theme(legend.position = "bottom") + 
  labs(color ="Aumento de ventana en")

sname <- "../Graficos_/alpha3.png"
ggsave(sname,width=12,height=7)

#(5) Grafico de dispersion
#-----------------------------------------
load(paste0(dirmother,"/Base/Base_duraciones_01.RData")) #Base de duraciones
nexchange_rate <-read_excel(path = paste0(dirmother,"/USDPEN2.xlsx"),
                            "Sheet 1") #Tipo de cambio
source("../TablaRegresion.R")

#Se elimina aquellas duraciones de precios menores a 5 dias y los censurados.
tabla_duraciones <- filter(tabla_duraciones, DDias >=5 )
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


#J = 0 s = 0 
#---------------
table1 <- grafreg(tabla_duraciones,nexchange_rate,0,0)
library(plotly)
plot_ly(table1, x = ~dER_m, y = ~Dpre_m, type = 'scatter', mode = 'markers', color = ~categoria, colors = 'Paired',
        marker = list(opacity = 0.5, sizemode = 'diameter')) %>%
  layout(xaxis = list(showgrid = FALSE),
         yaxis = list(showgrid = FALSE),
         showlegend = FALSE)


#J = 11 s = 0 
#---------------
table2 <- grafreg(tabla_duraciones,nexchange_rate,11,0)
ggplot(table2, aes(x = dER_m, y = Dpre_m)) +
  geom_point(colour = "darkslategray4") + theme_minimal()+ 
  geom_smooth(method ="lm")+
  labs(caption="Fuente: Elaboración propia", x = "Variación del tipo de cambio (mensual)", y ="Variación de los precios(mensual)")
sname <- "../Graficos_/scatter1.png"
ggsave(sname,width=12,height=7)

#J = 16 s = 10 
#---------------
table3 <- grafreg(tabla_duraciones,nexchange_rate,16,10)
ggplot(table3, aes(x = dER_m, y = Dpre_m)) +
  geom_point(colour = "darkslategray4") + theme_minimal()+ 
  geom_smooth(method ="lm")+
  labs(caption="Fuente: Elaboración propia", x = "Variación del tipo de cambio (mensual)", y ="Variación de los precios(mensual)")

sname <- "../Graficos_/scatter2.png"
ggsave(sname,width=12,height=7)
#J = 16 s = 20 
#---------------
table3 <- grafreg(tabla_duraciones,nexchange_rate,16,20)
ggplot(table3, aes(x = dER_m, y = Dpre_m)) +
  geom_point(colour = "darkslategray4") + theme_minimal()+ 
  geom_smooth(method ="lm")+
  labs(caption="Fuente: Elaboración propia", x = "Variación del tipo de cambio (mensual)", y ="Variación de los precios(mensual)")

sname <- "../Graficos_/scatter3.png"
ggsave(sname,width=12,height=7)
#J = 16 s = 30 
#---------------
table4 <- grafreg(tabla_duraciones,nexchange_rate,16,30)
ggplot(table4, aes(x = dER_m, y = Dpre_m)) +
  geom_point(colour = "darkslategray4") + theme_minimal()+ 
  geom_smooth(method ="lm")+
  labs(caption="Fuente: Elaboración propia", x = "Variación del tipo de cambio (mensual)", y ="Variación de los precios(mensual)")
sname <- "../Graficos_/scatter4.png"
ggsave(sname,width=12,height=7)
