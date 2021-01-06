CorrigeSubcategorias = function(serie){   
  
  #Se crea un a tabla donde las filas sean los productos y las columnas las categorias.
  serie=droplevels(serie)
  serie$subcategoria <- as.factor(serie$subcategoria)
  colnames_subcat <-levels(serie$subcategoria)
  
  resumen <- data.frame(as.matrix(ftable(serie$ID,serie$subcategoria))) # crea la tabla con todas los niveles
  setDT(resumen, keep.rownames = TRUE)[]
  
  #Dimension de la base resumen
  nn <- ncol(resumen)
  
  colnames(resumen)[2:nn]=colnames_subcat 
  colnames(resumen)[1]="ID"
  
  #Se cuenta el numero de clasificaciones que puedes tener un solo producto.
  resumen$aa <- rowSums(resumen[,2:nn]) #Suma por fila.
  resumen <- filter(resumen,aa!=0) #Eliminamos aquellos que no tienen clasificacion alguna.
  resumen <- resumen[,-(nn+1)] #Eliminamos el indicador anterior.
  
  for(i in 1:nrow(resumen)){
    resumen$contar[i]=length(which(resumen[i,2:nn]!=0))} #Contabiliza el número de clasificaciones por
  #cada producto.
  
  colnames(resumen) #Nombre de las subcategorias.
  table(resumen$contar) #Existen 38584 productos con unica clasificacion.
  
  #En la tabla capturamos todos los ids 
  resumen=filter(resumen,contar>=1)
  #vector de subcategorias
  subcategoria = colnames(resumen[,2:nn])
  #Se crea una columna donde se tenga el nivel máximo
  resumen$subcategoria_max=subcategoria[max.col(resumen[,2:nn])]
  
  #seleccionamos columnas utiles de resumen
  columnas_utiles=as.data.frame(select(resumen,c(ID,subcategoria_max)))
  columnas_utiles[,1]=as.numeric(columnas_utiles[,1])
  columnas_utiles[,2]=factor(columnas_utiles[,2])
  serie$ID=as.numeric(serie$ID)
  
  #añadimos una columna de subcategoria maxima a la base total
  base <- merge(serie, columnas_utiles, by ="ID")
  
  base$subcategoria <- base$subcategoria_max
  base <- base[,-16]
  
  #Ordenamos
  base <- select(base, c(2:11,1,12:15))
  base <- with(base, base[order(ID, fecha),])
  return(base)
}

