llenarnas = function(inserie){
  
  #numdias = 25
  numdias = 31
  #numdias = 40
  puntosna = which(is.na(inserie))
  
  "El objetivo es encontrar clusters de posiciones NA contiguas"
  
  nn = length(puntosna)
  
  "ahora rezagamos una posición"
  
  puntosnalag = c(0,puntosna[1:nn-1])
  
  "restamos ambos vectores, por definición los valores mayores a 1 identifican
  el inicio de una secuencia de NAs si están seguidos de 1's pero si no están seguidos de 1's son puntos aislados de NAs"
  
  ident_inicio = puntosna - puntosnalag
  
  seq_ini = which(ident_inicio>1) # esto nos da inicio de secuencia de NAs
  
  puntoslead = c(puntosna[2:nn],Inf)
  
  ident_fin = puntoslead-puntosna
  
  seq_fin = which(ident_fin>1)  # esto nos da fin de sequencie de NAs
  
  a=puntosna[seq_ini] 
  aa=puntosna[seq_fin]  
  
  if(length(aa)>length(a)){
    a = c(1,a)
  }
  
  agrupamientos = cbind(a,aa)
  
  N = nrow(agrupamientos)
  
  for(ii in 1:N){
    rango = agrupamientos[ii,]
    
    if(rango[1]>1 & rango[2]-rango[1]<=numdias){
      puntomedio = round(0.5*rango[1]+0.5*rango[2])
      rango_izq = rango[1]:puntomedio
      rango_der = (puntomedio+1):rango[2]
      precio_izq = inserie[rango[1]-1]
      precio_der = inserie[rango[2]+1]
      inserie[rango_izq] = precio_izq
      inserie[rango_der] = precio_der
    }
    
  }
  
  return(inserie)
}
