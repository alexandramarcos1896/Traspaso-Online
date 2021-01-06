rm(list=ls())

options(warn=0)
library(tidyverse)
library(readxl)
library(dplyr)
library(stringr)
options(warn=2)

directorio = dirname(rstudioapi::getSourceEditorContext()$path)
setwd(directorio)
dirmother = dirname(directorio)


"Apilamos las bases en una base mayor para el siguiente periodo"

ini_d = as.Date("2018-09-14",format="%Y-%m-%d")
fin_d = as.Date("2019-04-30",format="%Y-%m-%d")

fecha = ini_d

DATA =  vector(mode="numeric",length=0)

DATA = data.frame(pais=factor(),
                  tienda=factor(),
                  fecha=as.Date(character()),
                  categoria=character(),
                  subcategoria=character(),
                  marca=character(),
                  descripcion=character(),
                  precio_normal=numeric(),
                  precio_internet=numeric(),
                  precio_member=numeric(),
                  rating=numeric()) 

while(fecha <= fin_d){
  
  fname = paste0("Falabella_per_",format(fecha,"%Y-%m-%d"),".RData")
  fname = paste0("H:/SGInvEco/ALMACEN_TRABAJO/Alexandra/1.Bases iniciales/Actualizacion/Base/R1/",fname)
  
  if(file.exists(fname)){
    
    load(fname)
    
    # eliminamos items dobles (todas las variables iguales)
    
    dat=distinct(dat)
    
    
    # si el producto no tiene categoria, ponemos NA
    
    if(is.null(dat$categoria)){
      dat$categoria = NA
    }
    
    # si el producto no tiene subcategoria, ponemos NA
        if(is.null(dat$subcategoria)){
      dat$subcategoria = NA
        }
    

    
    # categoria y marca minusculo para no tener confusion 
    
    dat$categoria = str_to_lower(dat$categoria)
    dat$subcategoria=str_to_lower(dat$subcategoria)
    dat$marca = str_to_lower(dat$marca)
    dat$descripcion = str_to_lower(dat$descripcion)
    
    # hay productos con la variable grupo equivocada o que falta
    # asumimos que el error es sitematico, entonces eliminamos la variable
    
    #unique_dat = distinct(dat, categoria,subcategoria,grupo, marca, descripcion, precio_internet, precio_normal, precio_member,rating)
     unique_dat = distinct(dat, categoria,subcategoria,marca, descripcion, precio_internet, precio_normal, precio_member,rating)
    
    unique_dat$fecha = fecha
    data = unique_dat %>% select(fecha, categoria,subcategoria, marca, descripcion,
                                precio_normal, precio_internet, precio_member,
                                rating)
    # data = unique_dat %>% select(fecha, categoria,subcategoria,marca, descripcion, 
    #                              precio_normal, precio_internet, precio_member)
    # 
    
    # creamo base de datos con los datos de todos los dias considerados
    
    DATA = rbind(DATA,data)  
    
  }
  
  fecha = fecha + 1  
}


# eliminar datos 

rm(dat, data, unique_dat)



############# DETALLES BASE DE DATOS GENERAL ###################

DATA$pais  = as.factor("PER")
DATA$tienda = as.factor("falabella")

DATA = select(DATA,
              pais,
              tienda,
              fecha,
              categoria,
              subcategoria,
              marca,
              descripcion,
              precio_normal,
              precio_internet,
              precio_member,
              rating)


# eliminamos items sin descripci?n
DATA =  filter(DATA,!is.na(DATA$descripcion))
DATA =  filter(DATA,!is.na(DATA$precio_internet))

# eliminamos los simbolos no relevantes de las marcas y de las descripciones
DATA$descripcion = as.character(DATA$descripcion,width = NULL)
DATA$descripcion = str_replace_all(DATA$descripcion,"\n","") # replaces new line by space
DATA$descripcion = str_replace_all(DATA$descripcion,"\t","") # replaces new tab by space
DATA$descripcion = str_replace_all(DATA$descripcion,"\"","") # replaces ?? by space
DATA$descripcion = str_replace_all(DATA$descripcion,"''","") # replaces ?? by space
DATA$descripcion = str_trim(DATA$descripcion, side= "both") # deletes spaces in the end and in the beginning of the string
DATA$descripcion = str_to_lower(DATA$descripcion)

DATA$marca = str_replace_all(DATA$marca,"\n","")
DATA$marca = str_replace_all(DATA$marca,"\t","")
DATA$marca = str_trim(DATA$marca, side= "both")

DATA$categoria=str_replace_all(DATA$categoria,"\n","")
DATA$categoria=str_replace_all(DATA$categoria,"\t","")
DATA$categoria = str_trim(DATA$categoria, side= "both") # deletes spaces in the end and in the beginning of the string
DATA$categoria = str_to_lower(DATA$categoria)


DATA$subcategoria=str_replace_all(DATA$subcategoria,"\n","")
DATA$subcategoria=str_replace_all(DATA$subcategoria,"\t","")
DATA$subcategoria = str_trim(DATA$subcategoria, side= "both") # deletes spaces in the end and in the beginning of the string
DATA$subcategoria = str_to_lower(DATA$subcategoria)


# modificar marcas con errores

DATA = DATA %>% mutate(marca = recode(marca, 
                                      `black & deck` = "black and decker", 
                                      `black+decker` = "black and decker", 
                                      k?rcher = "karcher", 
                                      kasperky= "kaspersky",
                                      `lg electronics` = "lg",
                                      fuji="fujifilm",
                                      `kitchen aid`= "kitchenaid",
                                      `perfect choise` = "perfect choice", 
                                      practika = "practika",
                                      proair = "pro air", 
                                      shenhua = "shen hua",
                                      skil = "skill",
                                      swissgear = "swiss gear")) 

DATA = DATA %>% mutate(categoria = recode(categoria, 
                                      'cámara de fotos' = "fotografía", 
                                      tv = "televisores", 
                                      'tv-televisores' = "televisores", 
                                      'tv televisores' = "televisores",
                                      'línea blanca'="linea blanca",
                                      computadoras= "computadores"
                                  )) 
DATA = DATA %>% mutate(descripcion = recode(descripcion, 
                                            'error de foto plancha de cabello iht titanium' = "plancha de cabello iht titanium")) 

DATA=DATA %>%mutate(rating=recode(rating,
                                   `0  / 5`=0,
                                     `1  / 5`=1,
                                   `2  / 5`=2,
                                     `3  / 5`=3,
                                   `4  / 5`=4,
                                     `5  / 5`=5))

#elimina las filas donde las descripciones contienen su propia marca
DATA$marca=as.character(DATA$marca) 
DATA =  filter(DATA,!is.na(DATA$descripcion))

#Eliminamos la categoria videojuegos
DATA =  filter(DATA, categoria != "videojuegos")

fname_bd_rdata = paste0(dirmother,"/Base/",ini_d,"hasta",fin_d,".RData")
save(DATA,file=fname_bd_rdata)


