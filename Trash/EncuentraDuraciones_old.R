EncuentraDuraciones = function(in_dd){
  N = nrow(in_dd)
  
  delta = in_dd$precio_internet_completo[2:N]-in_dd$precio_internet_completo[1:(N-1)]
  
  aumentos = which(delta>0)+1
  
  disminuciones = which(delta<0)+1
  
  cambios = c(aumentos,disminuciones)
  cambios = sort(cambios)
  nc = length(cambios)
  "Ojo estos cambios deben estar ordenado"
  
  DDias =  vector(mode="numeric",length=0)
  Dini =  vector(mode="numeric",length=0)
  Dfin =  vector(mode="numeric",length=0)
  Dpre = vector(mode="numeric",length=0)
  
if(nc <= 1){
  DDias <- NA
  Dini = in_dd$fecha[cambios]
  Dfin <- NA
  Dpre <- NA
} else {
  for(ii in 2:nc){
    
    #Eliminamos la contabilizacion de sabados y domingos
    b = in_dd$fecha[cambios[ii]]
    a = in_dd$fecha[cambios[ii-1]]
    
    difdias = Nweekdays(a,b) #Preguntar (-1)
    
    vp = in_dd$precio_internet_completo[cambios[ii-1]:cambios[ii]]
    
    #volver a colocar difdias>5
    
    if (difdias>5 & sum(is.na(vp))==0){
      difdias = difdias
      dini = in_dd$fecha[cambios[ii-1]]
      dfin = in_dd$fecha[cambios[ii]]-1
      dpre = (in_dd$precio_internet_completo[cambios[ii]] - in_dd$precio_internet_completo[cambios[ii-1]])/in_dd$precio_internet_completo[cambios[ii-1]]
    } else {
      difdias <- NA
      dini <- in_dd$fecha[cambios[ii-1]]
      dfin <- NA
      dpre <- NA
    }
    DDias = rbind(DDias,difdias)
    Dini = rbind(Dini,dini)
    Dfin = rbind(Dfin,dfin)
    Dpre = rbind(Dpre,dpre)
  }
  
}
  #Agregamos id para diferenciar del resto de los items
  id = in_dd$marca_descrip_id[[1]]
  resultado = cbind.data.frame(DDias,Dini,Dfin,Dpre,id)
  resultado$Dini=as.Date(resultado$Dini, origin="1970-01-01")
  resultado$Dfin=as.Date(resultado$Dfin, origin="1970-01-01")
  return(resultado)
}

