completardias = function(basetotal){
  tabla = basetotal %>% group_by(ID) %>% distinct(ID)
  id = as.vector(unlist(tabla$ID))
  DD = vector(mode = "numeric", length = 0)
  for (ii in id) {
    bien = subset(basetotal, ID==ii)
    all_days_item = data.frame(fecha = seq.Date(from = min(bien$fecha), to = max(bien$fecha), by = "day"))
    bien = left_join(all_days_item, bien, by = "fecha", check.names=FALSE) #Check.names "importante"
    bien$pais = bien$pais[1]
    bien$tienda = bien$tienda[1]
    bien$categoria = bien$categoria[1]
    bien$subcategoria = bien$subcategoria[1]
    bien$marca = bien$marca[1]
    bien$descripcion = bien$descripcion[1]
    bien$ID = bien$ID[1]
    bien$id = bien$id[1]
    DD = rbind(DD,bien)
  }
  return(DD)
}