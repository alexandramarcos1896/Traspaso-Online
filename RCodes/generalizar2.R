generalizar2 = function(basetotal){
  id = basetotal %>% distinct(ID)
  avector = id[["ID"]]
  DD = vector(mode = "numeric", length = 0)
  for (ii in avector) {
    bien = filter(basetotal, ID==ii)
    dd <- EncuentraDuraciones(bien)
    if (length(dd)>=1){
      DD = rbind(DD,dd)
    }
  }
  return(DD)
}