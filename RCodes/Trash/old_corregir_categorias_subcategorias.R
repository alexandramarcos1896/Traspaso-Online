"Corregimos subategorias"
categoria_marca_descrip=df_tot1 %>% select(marca_descrip_id,categoria, subcategoria)
class(df_tot1$subcategoria)
df_tot1$subcategoria = as.factor(df_tot1$subcategoria)


"Corregir categoria y  subcategoria"
ids=select(df_tot1,marca_descrip_id) %>% distinct()
avector <- ids[,"marca_descrip_id"]

for (ii in avector) {
  aa=which(df_tot1$marca_descrip_id==ii)
  df_tot1$categoria[aa]=df_tot1$categoria[aa[1]]
  rm(aa)
}

for (ii in avector) {
  aa=which(df_tot1$marca_descrip_id==ii)
  df_tot1$subcategoria[aa]=df_tot1$subcategoria[aa[1]]
  rm(aa)
}
