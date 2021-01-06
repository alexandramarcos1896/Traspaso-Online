generalizar = function(basetotal){
  id = basetotal %>% distinct(ID)
  avector = id[["ID"]]
  DD = vector(mode = "numeric", length = 0)
  for (ii in avector) {
    bien = filter(basetotal, ID==ii)
    bien[,16] = llenarnas(bien[,16]) #Posicion de la columna de precios a completar
    DD = rbind(DD,bien)
  }
  return(DD)
}