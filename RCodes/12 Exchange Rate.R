
# Modificaciones al tipo de cambio ----------------------------------------

# (1)Limpiar memoria y cargar librerias -----------------------------------

graphics.off()
rm(list=ls())
library(dplyr)
library(rio)
library(stringr)

# (2) Base de Datos -------------------------------------------------------

nexchange_rate <-import("./input/USDPEN.xlsx")
sapply(nexchange_rate, class) #Revisamos los "types"

#Formato Date
nexchange_rate$fecha <- as.Date(nexchange_rate$fecha)

#Como no existe informacion del tipo de cambio en los dias feriados,sabados y
#domingos, se procede a crear una base con dias continuos y el proceso de
#llenado es con la funcion FILL.

all_days <- data.frame(fecha = seq.Date(from = min(nexchange_rate$fecha),
                                        to = max(nexchange_rate$fecha),
                                        by = "day"))

exchange_completed <- left_join(all_days, 
                                nexchange_rate,
                                by = "fecha",
                                check.names=FALSE)

exchange_completed <- exchange_completed %>% fill(USDPEN)

exchange_completed$dias <- weekdays(exchange_completed$fecha)

exchange_completed <- exchange_completed %>% filter(!dias %in% c("domingo","sábado")) %>%
  select(-dias)

export(exchange_completed, "./input/USDPEN2.xlsx")
