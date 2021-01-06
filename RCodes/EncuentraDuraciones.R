EncuentraDuraciones = function(in_dd){
  N = nrow(in_dd)
  
  delta = in_dd$precio_internet_completo[2:N]-in_dd$precio_internet_completo[1:(N-1)]
  
  aumentos = which(delta>0)+1
  
  disminuciones = which(delta<0)+1
  
  #Agregamos la ultima posicion para el proceso de censura
  final = nrow(in_dd)
  cambios = c(aumentos,disminuciones,final)
  cambios = sort(unique(cambios))
  nc = length(cambios)  #Ojo estos cambios deben estar ordenado
  
  DDias =  vector(mode="numeric",length=0)
  Dini =  vector(mode="numeric",length=0)
  Dfin =  vector(mode="numeric",length=0)
  Dpre = vector(mode="numeric",length=0)
  PInicial = vector(mode="numeric",length=0)

  if(nc == 2){
    a=in_dd$fecha[cambios[1]]
    b=in_dd$fecha[cambios[2]]
    DDias <- Nweekdays(a,b)-1
    Dini <- str_replace_all(a,"-","")
    Dfin <- str_replace_all(b,"-","")
    Dpre <- 0
    PInicial <- 0
    
    
  } else if (nc == 1) {
    a= first(in_dd$fecha)
    b= last(in_dd$fecha)
    DDias <- Nweekdays(a,b)-1
    Dini <- str_replace_all(a,"-","")
    Dfin <- str_replace_all(b,"-","")
    Dpre <- 0
    PInicial <- 0
 } else {
    for(ii in 2:nc){
      
      #Eliminamos la contabilizacion de sabados y domingos
      b = in_dd$fecha[cambios[ii]]
      a = in_dd$fecha[cambios[ii-1]]
      
      difdias = Nweekdays(a,b)-1 #Preguntar (-1)
      
      vp = in_dd$precio_internet_completo[cambios[ii-1]:cambios[ii]]
      
      #Elimine la condicion de  difdias>5 para poder conocer cuanto duran los precios para esta base de datos.
      #Luego pueden eliminarse para posibles estimaciones
      
      if (sum(is.na(vp))==0){
        difdias = difdias
        dini = in_dd$fecha[cambios[ii-1]]
        dini <- str_replace_all(dini,"-","")
        dfin = in_dd$fecha[cambios[ii]-1] #Mayor precision
        dfin <- str_replace_all(dfin,"-","")
        dpre = (in_dd$precio_internet_completo[cambios[ii]] - in_dd$precio_internet_completo[cambios[ii-1]])/in_dd$precio_internet_completo[cambios[ii-1]]
        pinic = in_dd$precio_internet_completo[cambios[1]]
        
      DDias = rbind(DDias,difdias)
      Dini = rbind(Dini,dini)
      Dfin = rbind(Dfin,dfin)
      Dpre = rbind(Dpre,dpre)
      PInicial = rbind(PInicial,pinic)
    }
    
    }
 }
  #Agregamos id para diferenciar del resto de los items
  id = in_dd$ID[[1]]
  resultado = cbind.data.frame(DDias,Dini,Dfin,Dpre,PInicial,id)
  resultado$Status = c(rep(0,nrow(resultado)-1),1)
  #resultado = cbind.data.frame(rbind(DDias,difdias),rbind(Dini,dini),rbind(Dfin,dfin),rbind(Dpre,dpre),id)
  #colnames(resultado) <- c("DDias", "Dini","Dfin","Dpre","ID")
  #resultado$Dfin=as.Date(resultado$Dfin, origin="1970-01-01")
  #resultado$Dini=as.Date(resultado$Dini, origin="1970-01-01")
  return(resultado)
}

