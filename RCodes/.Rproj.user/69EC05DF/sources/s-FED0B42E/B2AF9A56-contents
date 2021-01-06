
# Correccion de categorias y subcategorias --------------------------------

#Este script realiza la reduccion y uniformizacion de la clasificacion de los productos
#de la tienda SF, de tal manera que cuadren con la CLASIFICACION de la base de importaciones.

#(1)Limpiar memoria y cargar librerias
#----------------------------------------

rm(list = ls())
librerias <- c("tidyverse","dplyr","ggplot2","stringr", "tm", "data.table")
lapply(librerias, library, character.only =TRUE)


#(2) Base de Datos
load("./Base/MasterData/BasePERU_Proceso1.RData")
# 
# source("../CategoriasFun.R")
# source("../SubcategoriasFun.R")


#(4) Eliminamos subcategorias : VER TODO...
base <- base %>% mutate(subcategoria = as.character(subcategoria),
                        subcategoria = gsub("ver todo","",subcategoria),
                        subcategoria = gsub("-"," ",subcategoria),
                        subcategoria = str_trim(subcategoria,side = "both"),
                        subcategoria = stripWhitespace(subcategoria))

base <- base %>% mutate(subcategoria = as.character(subcategoria),
                        subcategoria = gsub("ver todo","",subcategoria),
                        subcategoria = str_trim(subcategoria,side = "both"),
                        subcategoria = stripWhitespace(subcategoria),
                        subcategoria = ifelse(str_detect(descripcion,"rack"),"accesorios tv",subcategoria),
                        subcategoria = ifelse(str_detect(descripcion,"tv led|combo led"),"tv led",subcategoria),
                        subcategoria = ifelse(str_detect(descripcion,"lcd"),"tv lcd",subcategoria),
                        subcategoria = ifelse(str_detect(descripcion,"audífono|audífonos|audifono"),"audífonos",subcategoria),
                        subcategoria = ifelse(str_detect(subcategoria,"refrigeracion"),"refrigeración",subcategoria),
                        subcategoria = ifelse(str_detect(subcategoria,"cocina"),"cocinas",subcategoria),
                        subcategoria = ifelse(str_detect(subcategoria,"linea blanca"),"línea blanca",subcategoria),
                        subcategoria = ifelse(str_detect(subcategoria,"ventiladores y aire acondicionado|ventiladores"),"ventilación y aire acondicionado",subcategoria),
                        subcategoria = ifelse(str_detect(subcategoria,"smartphones"),"celulares y smartphones",subcategoria),
                        subcategoria = ifelse(str_detect(subcategoria,"televisores|tv televisores"),"tv",subcategoria),
                        subcategoria = ifelse(str_detect(subcategoria,"teléfonos|telefonía"),"accesorios smartphone",subcategoria))

# Grupo de categorias --> Tecnologia
grupocat <- unique(base$categoria)

base$categoria = ifelse(base$categoria =="tecnología" & base$subcategoria %in% grupocat,base$subcategoria,base$categoria)

base$categoria = ifelse(base$categoria=="tecnología" & str_detect(base$subcategoria,"computadoras"),
                                              "computadores",base$categoria)
base$categoria = ifelse(base$categoria=="tecnología" & str_detect(base$subcategoria,"audífono"),
                            "audio",base$categoria)
base$categoria = ifelse(base$categoria=="tecnología" & str_detect(base$subcategoria,"telefonía"),
                            "teléfonos",base$categoria)
base$categoria = ifelse(base$categoria=="tecnología" & str_detect(base$subcategoria,"cámaras digitales"),
                            "fotografía",base$categoria)
base$categoria = ifelse(base$categoria=="tecnología" & str_detect(base$subcategoria,"tv lcd|accesorios tv"),
                            "televisores",base$categoria)

# Correcciones por categoria ---> Se crea una lista de categoria y subcategoria correcta
# con la finalidad de unir por subcategoria y corregir la categoria

lista3 <- base%>%select(-ID)%>%group_by(categoria,subcategoria)%>% summarize(repeticiones = n())%>%ungroup()
lista3 <- lista3 %>% group_by(subcategoria) %>% filter(repeticiones == max(repeticiones)) %>%
  ungroup()%>%
  mutate(categoria = ifelse(
    str_detect("(home theater)|(home theaters)|soundbars y home theaters|home theater y soundbar",subcategoria),"audio",categoria))%>%
  arrange(categoria)

# Agregar a la base principal con la lista corregida
base <- base %>% left_join(lista3 %>% select(-repeticiones), by = "subcategoria") %>%
  mutate(categoria = categoria.y)%>% select(-c(categoria.y,categoria.x))

# Falta alrreglar los que tiene misma cantidad de repeticiones
base_c <- base %>% left_join(base%>%group_by(ID,subcategoria)%>% summarise(repeticiones = n())%>%ungroup()%>%
                             group_by(ID)%>%filter(repeticiones ==max(repeticiones))%>%select(-repeticiones),
                           by = "ID")
lista4 <- lista4 %>% group_by(subcategoria) %>% filter(repeticiones == max(repeticiones)) %>%
  ungroup()%>%
  mutate(categoria = ifelse(str_detect("(home theater)|(home theaters)|soundbars y home theaters|home theater y soundbar",subcategoria),"audio",categoria))%>%
  arrange(categoria)

