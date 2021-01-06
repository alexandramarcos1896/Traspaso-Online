#----------------------
#Regresiones
#----------------------

"Directorio"
rm(list=ls())
directorio = dirname(rstudioapi::getSourceEditorContext()$path)
setwd(directorio)
dirmother = dirname(getwd())
#install.packages("stargazer")
library(stargazer)
library(ggplot2)
library(dplyr)


"Base de precios vs tc"

"Cargamos tabla de duraciones"
load(paste0(dirmother,"/Presentacion/Base_1.RData"))

tabla_duraciones = filter(tabla_duraciones, DDias>5)
#Cuadro de duraciones media por categoria y total con y sin censura manual

#Duracion de un precio en frecuencia mensual 
#Asumimos que un mes tiene 20 dias porque solo contabilizamos los dias laborables.
tabla_duraciones$DMensual = tabla_duraciones$DDias/20


"Sin censura manual"
Cuadro = tabla_duraciones %>% group_by(categoria) %>% summarise(mean = mean(DMensual), median = median(DMensual))
kable(Cuadro, "latex", booktabs = T)
tabla_duraciones %>% summarise(mean = mean(DMensual), median = median(DMensual)) #Para toda la base

"Con censura manual"
Cuadro1 = tabla_duraciones %>% filter(Status!=1) %>% filter(Dpre !=0) %>% group_by(categoria) %>%
  summarise(mean = mean(DMensual), median = median(DMensual))
kable(Cuadro1, "latex", booktabs = T)
tabla_duraciones %>% subset(Status!=1) %>% filter(Dpre !=0) %>% summarise(mean = mean(DMensual, na.rm = TRUE), median = median(DMensual, na.rm = TRUE))  #Para toda la base


#Relacion no lineal t vs t
#-----------------------
ggplot(tabla_duraciones, aes(x = abs(dER_m), y = abs(Dpre_m))) +
  geom_point(aes(color = factor(Status)))  +
  labs(caption="Fuente: Elaboración propia", x = "Variacion del tipo de cambio", y ="Variacion de los precios")

#Heterocedaticidad t vs t
#-----------------------
ggplot(tabla_duraciones, aes(x = dER_m, y = Dpre_m)) +
  geom_point(colour = "darkslategray4") + theme_minimal()+ 
  geom_smooth(method ="lm")+
  labs(caption="Fuente: Elaboración propia", x = "Variación del tipo de cambio (mensual)", y ="Variación de los precios(mensual)")

ggplot(tabla_duraciones, aes(x = abs(dER_m), y = inv_dtc)) +
  geom_point(colour = "darkslategray4") + theme_minimal()+ 
  geom_smooth(method ="lm")+
  labs(caption="Fuente: Elaboración propia", x = "Variación del tipo de cambio (mensual)", y ="Variación de los precios(mensual)")

#La variacion del tipo de cambio ha sido 0 y no han cambiado
#------------------------
tabla_duraciones <- tabla_duraciones %>% filter(Status==0) %>% filter(dER_m != 0)

#Primeras regresiones
#------------------------

tabla_duraciones$abs_tc <- abs(tabla_duraciones$dER_m)
tabla_duraciones$inv_dtc <- 1/tabla_duraciones$abs_tc

#---------------------------------------------------
reg1 <- lm(abs(Dpre_m) ~ I(1/abs_tc), data = tabla_duraciones)
summary(reg1)
stargazer(reg1)


#ggplotRegression(reg1)

electrohogar <- subset(tabla_duraciones, categoria == "electrohogar")
reg2 <- lm(Dpre_m~ dER_m  + DDias_dER, data = electrohogar)
summary(reg2)

tecno <- subset(tabla_duraciones, categoria == "tecnología")
reg2 <- lm(Dpre_m~ dER_m  + DDias_dER, data = tecno)
summary(reg2)

tele <- subset(tabla_duraciones, categoria == "televisores")
reg2 <- lm(Dpre_m~ dER_m  + DDias_dER, data = tele)
summary(reg2)

electrod <- subset(tabla_duraciones, categoria == "electrodomésticos")
reg2 <- lm(Dpre_m~ dER_m, data = electrod)
summary(reg2)

telefonos <- subset(tabla_duraciones, categoria == "teléfonos")
reg2 <- lm(Dpre_m~ dER_m + DDias_dER, data = telefonos)
summary(reg2)

lineab <- subset(tabla_duraciones, categoria == "linea blanca")
reg2 <- lm(Dpre_m~ dER_m + DDias_dER, data = lineab)
summary(reg2)

computadores <- subset(tabla_duraciones, categoria == "computadores")
reg2 <- lm(Dpre_m~ dER_m + DDias , data = computadores)
summary(reg2)

#------------------------------------------------------------------
#Sin constante pero incluyendo todas las dummies



library(dummies)
tabla_duraciones <- cbind(tabla_duraciones, dummy(tabla_duraciones$categoria, sep="_"))
tabla_duraciones$DDias_dER <- tabla_duraciones$DDias*tabla_duraciones$dER_m

reg2 <- lm(tabla_duraciones$Dpre_m~ 0 +tabla_duraciones$dER_m+ tabla_duraciones$DDias_dER +
             tabla_duraciones$tabla_duraciones_audio +
             tabla_duraciones$tabla_duraciones_computadores +
             tabla_duraciones$tabla_duraciones_electrodomésticos+
             tabla_duraciones$tabla_duraciones_electrohogar+
             tabla_duraciones$tabla_duraciones_fotografía+
             tabla_duraciones$`tabla_duraciones_linea blanca`+
             tabla_duraciones$tabla_duraciones_tecnología+
             tabla_duraciones$tabla_duraciones_teléfonos+
             tabla_duraciones$tabla_duraciones_televisores)
summary(reg2)
stargazer(reg2)

reg2 <- lm(Dpre_m~ 0 + DDias_dER +
             tabla_duraciones_audio +
             tabla_duraciones_computadores +
             tabla_duraciones_electrodomésticos+
             tabla_duraciones_electrohogar+
             tabla_duraciones_fotografía+
             `tabla_duraciones_linea blanca`+
             tabla_duraciones_tecnología+
             tabla_duraciones_teléfonos+
             tabla_duraciones_televisores, data = base_98)

#-------------------------
#Modelo simple ERPT corto plazo
#-------------------------


reg3 <- lm(tabla_duraciones$Dpre_m ~ 0 +tabla_duraciones$dER_m +
             tabla_duraciones$tabla_duraciones_audio +
             tabla_duraciones$tabla_duraciones_computadores +
             tabla_duraciones$tabla_duraciones_electrodomésticos+
             tabla_duraciones$tabla_duraciones_electrohogar+
             tabla_duraciones$tabla_duraciones_fotografía+
             tabla_duraciones$`tabla_duraciones_linea blanca`+
             tabla_duraciones$tabla_duraciones_tecnología+
             tabla_duraciones$tabla_duraciones_teléfonos+
             tabla_duraciones$tabla_duraciones_televisores)

a <- summary(reg3)
stargazer(reg3, align = TRUE)
#kable(a, "latex", bookmark = T)

#------------------------------
#Creamos dummies para la apreciacion y depreciacion

tabla_duraciones$A <- ifelse(tabla_duraciones$dER_m <0 ,1,0)
tabla_duraciones$D <- ifelse(tabla_duraciones$dER_m >0 ,1,0)

#tabla_duraciones <- cbind(tabla_duraciones, dummy(tabla_duraciones$categoria, sep="_"))
#tabla_duraciones$DDias_1 = tabla_duraciones$DDias[]
reg4 <- lm(Dpre_m ~ dER_m + DDias ,data =tabla_duraciones)
reg4 <- lm(Dpre_m ~ dER_m + A,data =tabla_duraciones)
summary(reg4)
stargazer(reg4)

#Sin constante pero incluyendo ambas dummies depreciacion y apreciacion

reg5 <- lm(Dpre_m ~  Dpre_m ,data =tabla_duraciones)
reg5 <- lm(Dpre_m ~  dER_m + D+ D*dER_m ,data =tabla_duraciones)
summary(reg5)
stargazer(reg5)

#----------------------------------------------
#Regresion tomando en consideracion el tamaño del cambio
#Criterio de POLLARD Y COUGHLIN (2004) PASSTROUGHT DE PRECIOS DE LAS IMPORTACIONES AMERICANAS
tabla_duraciones$L <- ifelse(abs(tabla_duraciones$dER_m) > 0.03 ,1,0)
tabla_duraciones$S <- ifelse(abs(tabla_duraciones$dER_m) < 0.03 ,1,0)


reg5 <- lm(Dpre_m ~ 0 + dER_m + L + S,data =tabla_duraciones)
summary(reg5)
stargazer(reg5)


#Errores heterocedasticos, correccion de la matriz de varianzas y covarianzas.
#-----------------------------------



#-------------------------
#Segunda regresion modelo TOBIT (IDEA)
#-------------------------




#Tercera regresion modelo no lineal y algunos controles como caracteristicas del propio bien
#-----------------------------  


library(gamlss)
library(evir)
library(gamlss.tr)

dd = tabla_duraciones

gen.trun(par = c(0.03,-0.03), family = "NO", type = c("both"))

m1 <- gamlss(dd$Dpre_m  ~  dd$dER_m + dd$categoria + dd$DDias + dd$DDias*dd$dER_m ,
             sigma.fo =~ dd$dER_m + dd$categoria  + dd$DDias + dd$DDias*dd$dER_m , data=dd, family=NO)


m2 <- gamlss(dd$Dpre_m  ~  dd$dER_m + dd$categoria + dd$DDias + dd$DDias*dd$dER_m ,
             sigma.fo =~ dd$dER_m + dd$categoria  + dd$DDias + dd$DDias*dd$dER_m , data=dd, family=SN1)
refit(m2)

#------------------------------------------------------------------------
#Graficos  // Tasas anuales // Tasas Mensuales
#------------------------------------------------------------------------

#Histograma del tamaño de los cambios // duraciones
#-------------------------------------------------------------
ggplot(tabla_duraciones) +
  geom_density(aes(x=abs(Dpre_m),y=..density.. ,colour=categoria),size=1,kernel = "epanechnikov",adjust=1/5) +
  xlim(0,5)

ggplot(tabla_duraciones %>% filter(Status == 0)) +
  geom_density(aes(x=abs(Dpre_m),y=..density.. ,colour=categoria),size=1,kernel = "epanechnikov",adjust=1/5) +
  xlim(0,5)
#Conclusion muy disperso no hay una tendencia en los cabias de precios


#Graficos de dispersion // Tasas Mensuales
#-------------------------------------------------------------
theme_set(theme_minimal())

ggplot(tabla_duraciones, aes(x = abs(d_ER), y = abs(Dpre))) +
  geom_point() +
  labs(caption="Fuente: Elaboración propia", x = "Variacion del tipo de cambio (mensual)", y ="Variacion de los precios(mensual)")

ggplot(tabla_duraciones, aes(x = abs(dER_m), y = abs(Dpre_m))) +
  geom_point(aes(color = factor(categoria))) + geom_smooth(method = "lm")
labs(caption="Fuente: Elaboración propia", x = "Variacion del tipo de cambio (mensual)", y ="Variacion de los precios(mensual)")

ggplot(tabla_duraciones, aes(x = abs(dER_a), y = abs(Dpre_a))) +
  geom_point(aes(color = factor(categoria))) + geom_smooth(method = "lm")
labs(caption="Fuente: Elaboración propia", x = "Variacion del tipo de cambio (mensual)", y ="Variacion de los precios(mensual)")

#Graficos de dispersion (Solo apreciaciones)// Tasas Mensuales
#-------------------------------------------------------------
ggplot(tabla_duraciones %>% filter(dER_m < 0), aes(x = dER_m, y = Dpre_m)) +
  geom_point(aes(color = factor(categoria))) +
  labs(caption="Fuente: Elaboración propia", x = "Variacion del tipo de cambio (mensual)", y ="Variacion de los precios(mensual)")

ggplot(tabla_duraciones %>% filter(dER_m > 0), aes(x = abs(dER_m), y = abs(Dpre_m))) +
  geom_point(aes(color = factor(categoria))) +
  labs(caption="Fuente: Elaboración propia", x = "Variacion del tipo de cambio (mensual)", y ="Variacion de los precios(mensual)")

#------------------------------------------------------------------------
#Graficos de dispersion //  Diferenciando por variables censuradas
#------------------------------------------------------------------------

ggplot(tabla_duraciones, aes(x = abs(dER_m), y = abs(Dpre_m))) +
  geom_point(aes(color = factor(Status)))  +
  labs(caption="Fuente: Elaboración propia", x = "Variacion del tipo de cambio", y ="Variacion de los precios")

ggplot(tabla_duraciones, aes(x = abs(d_ER), y = abs(Dpre))) +
  geom_point(aes(color = factor(Status),  size=Status)) +
  labs(caption="Fuente: Elaboración propia", x = "Variacion del tipo de cambio", y ="Variacion de los precios") +
  theme(legend.position = "none")

ggplot(tabla_duraciones, aes(x = d_ER, y = Dpre)) +
  geom_point(aes(color = factor(Status),  size=Status)) + geom_smooth(method = "lm") +
  labs(caption="Fuente: Elaboración propia", x = "Variacion del tipo de cambio", y ="Variacion de los precios") +
  theme(legend.position = "none")

#CONCLUSION DE LOS GRAFICOS ANTERIOR EL PT ES NO  LINEAL

#Graficos de dispersion // Tasas Mensuales
#-------------------------------------------------------------
library(plotly)
plot_ly(tabla_duraciones, x = ~abs(dER_m), y = ~abs(Dpre_m), type = 'scatter', mode = 'markers', color = ~categoria, colors = 'Paired',
        marker = list(opacity = 0.5, sizemode = 'diameter')) %>%
  layout(xaxis = list(showgrid = FALSE),
         yaxis = list(showgrid = FALSE),
         showlegend = FALSE)
