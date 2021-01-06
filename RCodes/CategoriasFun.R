CorrigeCategorias = function(serie){   
  
  #Se crea un a tabla donde las filas sean los productos y las columnas las categorias.
  serie=droplevels(serie)
  serie$categoria <- as.factor(serie$categoria)
  colnames_cat=levels(serie$categoria)  # 
  resumen=data.frame(as.matrix(ftable(serie$ID,serie$categoria))) # crea la tabla con todas los niveles
  setDT(resumen, keep.rownames = TRUE)[]
  
  #Dimension de la base resumen:
  nn <- ncol(resumen)
  
  colnames(resumen)[2:nn]=colnames_cat 
  colnames(resumen)[1]="ID"
  
  #Se cuenta el numero de clasificaciones que puedes tener un solo producto.
  resumen$aa=rowSums(resumen[,2:nn]) #Suma por fila.
  resumen=filter(resumen,aa!=0) #Eliminamos aquellos que no tienen clasificacion alguna.
  resumen=resumen[,-(nn+1)] #Eliminamos el indicador anterior.
  
  #ln <- nn-1
  for(i in 1:nrow(resumen)){
    resumen$contar[i]=length(which(resumen[i,2:nn]!=0))} #Contabiliza el número de clasificaciones por
  #cada producto.
  
  colnames(resumen) #Nombre de las categorias.
  table(resumen$contar) #Existen 43838 productos con unica clasificacion.
  
  #En la tabla capturamos todos los ids 
  resumen=filter(resumen,contar>=1)
  #vector de categorias
  categoria = colnames(resumen[,2:nn])
  #Se crea una columna donde se tenga el nivel máximo
  resumen$categoria_max=categoria[max.col(resumen[,2:nn])]
  
  #seleccionamos columnas utiles de resumen
  columnas_utiles=as.data.frame(select(resumen,c(ID,categoria_max)))
  columnas_utiles[,1]= as.numeric(columnas_utiles[,1])
  columnas_utiles[,2]=factor(columnas_utiles[,2])
  serie$ID=as.numeric(serie$ID)
  
  #añadimos una columna de categoria maxima a la base total
  base <- merge(serie, columnas_utiles, by ="ID") 
  
  base$categoria <- base$categoria_max
  base=base[,-16]
  
  base <- select(base, c(2:11,1,12:15))
  base <- with(base, base[order(ID, fecha),])
  return(base)
}

