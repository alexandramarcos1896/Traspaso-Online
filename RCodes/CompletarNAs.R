"UNION"
rm(list=ls())
directorio = dirname(rstudioapi::getSourceEditorContext()$path)
setwd(directorio)
dirmother = dirname(getwd())


options(warn=0)
library(tidyverse)
#library(sqldf)
library(readxl)
library(dplyr)
library(stringr)
library(data.table)
library(stringr)
#library(compare)
library(ggplot2)
options(warn=2)
source("llenarnas.R")


load("04_Base_alterada.RData")

dd = DD %>% filter(marca_descrip_id == "9214")

"Introducimos la serie que vamos a analizar"

seriellenar = dd$precio_internet

seriellenada = llenarnas(seriellenar)


x = cbind(seriellenar,seriellenada)

plot(seriellenar,col="blue")
lines(seriellenada,col="red")
