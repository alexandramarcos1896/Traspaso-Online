
# Correccion de categorias y subcategorias --------------------------------

#Este script realiza la reduccion y uniformizacion de la clasificacion de los productos
#de la tienda SF, de tal manera que cuadren con la CLASIFICACION de la base de importaciones.


#(1)Limpiar memoria y cargar librerias -------------------------------------

rm(list = ls())
librerias <- c("tidyverse","dplyr","ggplot2","stringr", "tm", "data.table","rio")
lapply(librerias, library, character.only =TRUE)

# (2) Base de Datos -------------------------------------------------------


load("../output/MasterData/BasePERU_Proceso1.RData")


# (3) Eliminamos subcategorias : VER TODO... ------------------------------

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


# (4) Grupo de categorias --> Tecnologia ----------------------------------

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


#(5) Corregimos duplicidad en la clasificacion de las subcategorias ----------


lista4 <- base %>%group_by(ID,subcategoria)%>% summarise(repeticiones = n()) %>%
  ungroup()

lista3 <- lista4 %>% filter(ID %in% lista_repeticiones$ID)

lista5 <- lista4 %>%
  mutate(subcategoria = ifelse(str_detect(subcategoria,"tv"),"tv led",subcategoria),
         subcategoria = ifelse(str_detect(subcategoria,"home theater"),"home theater y soundbar",subcategoria),
         subcategoria = ifelse(str_detect(subcategoria,"laptops 2 en 1|all in one|gamers"),"laptops",subcategoria),
         subcategoria = ifelse(str_detect(subcategoria,"mochilas para laptop"),"accesorios computación",subcategoria),
         subcategoria = ifelse(str_detect(subcategoria,"celulares desbloqueados"),"celulares y smartphones",subcategoria),
         subcategoria = ifelse(str_detect(subcategoria,"tablets"),"computadoras y tablets",subcategoria)) %>%
  group_by(ID,subcategoria)%>% summarise(repeticiones = sum(repeticiones)) %>%
  ungroup()

lista5 <- lista5 %>% ungroup()%>%
  group_by(ID)%>%filter(repeticiones ==max(repeticiones))%>%select(-repeticiones)

lista4_A <- lista5 %>% group_by(ID) %>% mutate(n = n()) %>% filter(n==1)
lista4_B <- lista5 %>% group_by(ID) %>% mutate(n = n()) %>% filter(n!=1) %>%
  filter(!subcategoria %in% c("electrodomésticos","computadoras",
                             "home theater y soundbar","accesorios celulares",
                             "audio","cámara de fotos")) %>% group_by(ID) %>% mutate(n = n()) %>%
  filter(n==1)

lista4_C <- lista5 %>% group_by(ID) %>% mutate(n = n()) %>% filter(n!=1) %>%
  filter(!subcategoria %in% c("electrodomésticos","computadoras",
                              "home theater y soundbar","accesorios celulares",
                              "audio","cámara de fotos")) %>% group_by(ID) %>% mutate(n = n()) %>%
  filter(n!=1)

lista4_D <- lista4_C %>% arrange(ID) %>% group_by(ID) %>% mutate(n = seq(1:n())) %>%
  filter(n==2)

# Unimos la nueva lista

lista_subcategoria <- lista4_A %>% rbind(lista4_B) %>% rbind(lista4_D) %>% select(-n)
length(unique(lista_subcategoria$ID))

# Incorporamos a la base de datos

base_c <- base %>% left_join(lista_subcategoria,by = "ID") %>%
  mutate(subcategoria = subcategoria.y)%>% select(-c(subcategoria.y,subcategoria.x))
length(unique(base$ID))


# (6) Exportamos lista de categoria y subcategoria ----------------

base_clasificacion <- base_c %>%ungroup()%>%select(categoria,subcategoria) %>%
  unique() %>% arrange(categoria)

export(base_clasificacion,"../output/Adicionales/Lista_Clasificadores.xlsx")
# Se puede mejorar la clasificacion ojo

# (7) Guardamos base de datos ---------------------------------------------

base <- base_c %>% select(pais,tienda,fecha,ID,categoria,subcategoria,marca,descripcion,
                          precio_normal,precio_internet,precio_member,precio_normal_completo,
                          precio_member_completo,rating,id)

save(base,file= "../output/MasterData/Base_Final_Peru_2.RData")
