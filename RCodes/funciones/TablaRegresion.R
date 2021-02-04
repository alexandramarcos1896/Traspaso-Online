grafreg <- function(baseduracion,basetipocambio,j,s){
    
  #Primero se crea la fecha final con un numero "j" de rezagos deseado:
  baseduracion$Dfin_j <- baseduracion$Dfin - j
  #Se identifica los nuevos dias.
  baseduracion$DfinDias <- weekdays(as.Date(baseduracion$Dfin_j))
  #Para aquellos que son sabado o domingo....
  domingoF <- which(str_detect(baseduracion$DfinDias,"domingo"))
  sabadoF <- which(str_detect(baseduracion$DfinDias,"sábado"))
  #....se retrocede uno o dos dias respectivamente.
  baseduracion$Dfin_j[domingoF] <- baseduracion$Dfin_j[domingoF]-2
  baseduracion$Dfin_j[sabadoF] <- baseduracion$Dfin_j[sabadoF]-1
  
  #Se identifica la diferencia de dias inicial.
  #El negativo indica resta de dias.
  k <- -baseduracion$DDias
  
  #La nueva fecha final, se le resta la duración "k" y el aumento 
  #de ventana "s".
  baseduracion$Dini_s <- offset(baseduracion$Dfin_j,k-s+1,cal = CalPeru)
  
  baseduracion <- baseduracion[,-c(11)]
  
  #Inicia proceso de matchado:
  #-----------------
  baseduracion <- merge(baseduracion, basetipocambio,
                        by.x="Dini_s", by.y="fecha", sort = FALSE, all.x = TRUE)
  names(baseduracion)[12] <- "ER_inicial"
  baseduracion_total <- merge(baseduracion, basetipocambio,
                              by.x="Dfin_j", by.y="fecha", sort = FALSE, all.x = TRUE)
  names(baseduracion_total)[13] <- "ER_final"
  
  #Ordenamos
  #-----------------
  baseduracion <- with(baseduracion_total,
                       baseduracion_total[order(id,Dini_s),])
  baseduracion <- select(baseduracion, c(3:6,2,1,7:13)) 
  rownames(baseduracion) <- c(1:nrow(baseduracion))
  rm(baseduracion_total)
  
  #Se identifica los tipo de cambio donde no ha matchado y se eliminan.
  #--------------------------------------------------------------------
  #No deberian existir no matches
  
  baseduracion <- baseduracion[!is.na(baseduracion$ER_inicial),]
  baseduracion <- baseduracion[!is.na(baseduracion$ER_final),]
  
  #Calculamos la variacion cambiaria
  #----------------------------------
  baseduracion$d_ER <- round((baseduracion$ER_final /
                                baseduracion$ER_inicial)-1, 7) 
  
  #Variaciones de precios y tipo de cambio mensualizada
  #----------------------------------------------------
  "Depre Mensualizada"
  baseduracion$Dpre_m <- (baseduracion$Dpre*20)/baseduracion$DDias
  "ER Mensualizada"
  baseduracion$dER_m <- (baseduracion$d_ER*20)/baseduracion$DDias
  
  return(baseduracion)
}