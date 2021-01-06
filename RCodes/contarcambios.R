contarcambios = function(basetotal){
  tabla = basetotal %>% group_by(ID) %>% distinct(ID) %>% mutate(ncambios = NA)
  id = as.vector(unlist(tabla$ID))
  contar = 1
  for (ii  in id) {
    dd = filter(basetotal, ID == ii)
    N = nrow(dd)
    delta = dd$precio_internet[2:N]-dd$precio_internet[1:(N-1)]
    delta[delta!=0] <- 1
    tabla[contar,2] = sum(delta)
    contar = contar + 1
  }
  return(tabla)
}
