regresion2 <- function(baseduracion,basetipocambio,j,s,y){
  
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
  
  baseduracion <- baseduracion[,-c(13)]
  
  #Inicia proceso de matchado:
  #-----------------
  baseduracion <- merge(baseduracion, basetipocambio,
                            by.x="Dini_s", by.y="fecha", sort = FALSE, all.x = TRUE)
  names(baseduracion)[14] <- "ER_inicial"
  baseduracion_total <- merge(baseduracion, basetipocambio,
                              by.x="Dfin_j", by.y="fecha", sort = FALSE, all.x = TRUE)
  names(baseduracion_total)[15] <- "ER_final"
  
  #Ordenamos
  #-----------------
  baseduracion <- with(baseduracion_total,
                       baseduracion_total[order(id,Dini_s),])
  #baseduracion <- select(baseduracion, c(3:6,2,1,7:13))
  baseduracion <- baseduracion[,c(3:6,2,1,7:15)]
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
  #baseduracion$Dpre_m <- (baseduracion$Dpre*20)/baseduracion$DDias
  "ER Mensualizada"
  #baseduracion$dER_m <- (baseduracion$d_ER*20)/baseduracion$DDias
  "DDias a DMeses"
  #baseduracion$DMeses <- baseduracion$DDias/20
  
  #Eliminamos categorias vacias
  #-----------------------------
  baseduracion$categoria <- droplevels(baseduracion$categoria)
  
  #Duracion
  #-----------
  baseduracion$DDias_dER <- baseduracion$DDias*baseduracion$d_ER
  
  #Frecuencia
  #-----------
  frecuencia <- baseduracion %>% count(id)
  meanfrequency <- mean(frecuencia$n)
  baseduracion <- merge(baseduracion,frecuencia, by = "id")
  
  baseduracion$demeanedfreq <- baseduracion$n - meanfrequency
  baseduracion$ER_freq <- baseduracion$demeanedfreq*baseduracion$d_ER
  
  #Explicativas adicionales:
  #--------------------------

  #Dummy para rebajas
  #----------------------
  baseduracion$delta <- -0.037
  baseduracion$control <- ifelse(baseduracion$Dpre < baseduracion$delta,1,0 )
  #baseduracion$DMesesdER <- baseduracion$DMeses*baseduracion$dER_m
  
  #Control por tamaño(precio inicial)
  #-----------------------------------
  baseduracion$ERPrecio <- baseduracion$PInicial*baseduracion$Dpre
  
  
  #Regresion OLS
  #--------------
  reg1 <- lm(Dpre ~ 0 + d_ER +ERPrecio + PInicial+ control + DDias +
               categoria,
             data = baseduracion)
  
  #Regresion FGLS
  #--------------
  reg2 <- lm(Dpre ~ 0 + d_ER +ERPrecio + PInicial+ control + DDias +
             categoria,
             data = baseduracion,
             weights = 1/reg1$fitted.values^2)
  
  
  #BP(HETEROCEDASTICIDAD) SI HAY
  #bp <- bptest(reg1, ~ dER_m*DMesesdER + I(DMesesdER^2) + I(dER_m^2), data = baseduracion)
  #bp$p.value
  #DW (AUTOCORRELACION) SI HAY
  #dwtest(reg1,alternative = "two.sided",iterations = 1000)
  
  #vcov(reg1)
  #ggplot() + geom_point(aes(x = baseduracion$dER_m, y = reg1$residuals^2))
  
    #Extraccion de coeficientes
  #---------------------------
  valores <- summary(reg2)
  alpha   <- valores$coefficients[1,1]
  betha   <- valores$coefficients[1,4]
  #gamma   <- valores$coe
  std.err <- valores$coefficients[1,2]
  tvalue  <- valores$coefficients[1,3]
  
  if(y==1){
    datos <- c(alpha,std.err,tvalue)}
  if(y==2){
    datos <- list(valores,baseduracion)
  }

  
  return(datos)
  
}