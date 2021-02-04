
# Estimacion --------------------------------------------------------------

#El objetivo de este script es poder arrojar una matriz que contenga el coeficiente
#de traspaso hacia los precios de internet. Este depende de distintos
#intervalos de distancia.. j y k.


# (1)Limpiar memoria y cargar librerias -----------------------------------

rm(list = ls())
librerias <- c("dplyr","ggplot2","stringr","rio","bizdays")
lapply(librerias,library, character.only = TRUE)

# (2)Funciones ------------------------------------------------------------
source("./funciones/regresion.R")

# (3) Base de Datos -------------------------------------------------------
load("../output/MasterData/Base_duraciones_01.RData")
nexchange_rate <-import("./input/USDPEN2.xlsx")


# (4) Proceso -------------------------------------------------------------
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


# (5) coefficient matrix --------------------------------------------------

#Matrix final
alpha_matrix <- numeric(length = 0L)
std_matrix <- numeric(length = 0L)
tvalue_matrix <- numeric(length = 0L)
#Hasta 30 dias atrás
for (kk in 0:30) {
  base1 <- numeric(length = 0L)
  base2 <- numeric(length = 0L)
  base3 <- numeric(length = 0L)
  for (jj in 0:30) {
    estimacion <- regresion(tabla_duraciones,nexchange_rate,jj,kk,1)
    valor1 <- estimacion[1]
    valor2 <- estimacion[2]
    valor3 <- estimacion[3]
    base1 <- rbind(base1,valor1)
    base2 <- rbind(base2,valor2)
    base3 <- rbind(base3,valor3)
  }
  alpha_matrix <- cbind(alpha_matrix,base1) 
  std_matrix <- cbind(std_matrix,base2)
  tvalue_matrix <- cbind(tvalue_matrix,base3)
}

#Las filas son jj.
#Las columnas son kk
rownames(tvalue_matrix) <- c(0:30)
colnames(tvalue_matrix) <- c(0:30)

rownames(std_matrix) <- c(0:30)
colnames(std_matrix) <- c(0:30)

rownames(alpha_matrix) <- c(0:30)
colnames(alpha_matrix) <- c(0:30)

#(5)Guardamos
#------------------
file_name <- "../output/MasterData/matrix_regresiones.RData"
save(alpha_matrix,std_matrix,tvalue_matrix, file = file_name)

