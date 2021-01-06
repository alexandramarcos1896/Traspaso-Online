rm(list=ls())

# Process_All -------------------------------------------------------------

# Este script tiene por finalidad cargar los rdatas crudos (RAW) para su proceso de limpieza y
# estructura.

# Estas bases resultantes guardan en la carpeta R1.

# Librerias
library(tidyverse)
library(stringr)

# Se establece la fecha de inicio y fin de las bases crudas
options(warn=2)

ini_d = as.Date("2018-09-17",format="%Y-%m-%d")
fin_d = as.Date("2019-04-30",format="%Y-%m-%d")

#2018-08-01 i
#2018-09-13 f

#2018-03-30 i
#2018-07-31 f

#2018-01-02 i
#2018-03-30 f

#2017-09-21 i
#2018-01-01 f

fecha = ini_d

while(fecha <= fin_d){
  fname = paste0("./Base/raw/falabella_per_",format(fecha,"%Y-%m-%d"),".RData")
  
  if(file.exists(fname)){
    load(fname)
    
    data=distinct(DD)
    
    data$precio_normal = NA
    data$precio_internet = NA
    data$precio_member = NA
    
    
    # eliminamos filas sin precio (no hay)
    
    if(length(data$price)==0){
      names(data)[names(data) == "precios"] <- "price"
    }
    
    data = filter(data,!is.na(data$price))
    
    # todo minusculo
    
    data$price = str_to_lower(data$price)
    
    nro_precios = str_count(data$price, "s/")
    
    index_un_precio     = which(nro_precios == 1)
    index_dos_precios   = which(nro_precios == 2)
    index_tres_precios  = which(nro_precios == 3)
    
    # productos con un precio (default: es el precio_internet)
    
    data$precio_internet[index_un_precio]= data$price[index_un_precio]
    data$precio_internet[index_un_precio]= gsub("[^0-9.]", "", data$precio_internet[index_un_precio])
    
    # separamos los precios de los productos con dos precios
    
    soles_dos = str_locate_all(data$price[index_dos_precios], "s/")   # lista de loc. de simbolos s/
    int_dos   = str_locate(data$price[index_dos_precios], "internet") # lista de loc. de la palabra internet
    
    for (i in 1:length(soles_dos)){   # recorre la lista de localizaciones
      
      j = index_dos_precios[i]
      
      # si la palabra "normal" aparece, asumimos que el primer precio es el de internet, y el segundo el normal (vease tabla)
      
      if(str_count(data$price[j], "normal")==1){
        #if( int_dos[i, 2] < soles_dos[[i]][2,1]){
        
        
        data$precio_internet[j] = substr(data$price[j], soles_dos[[i]][1,2]+1, int_dos[i,1]-1)
        data$precio_normal[j]   = substr(data$price[j], soles_dos[[i]][2,2]+1, nchar(data$price[j]))
        
        if(grepl("[-]", data$precio_internet[j])){
          
          dash = str_locate(data$precio_internet[j], "[-]")
          precio1 = substr(data$precio_internet[j], 1, dash[1,1]-1)
          precio2 = substr(data$precio_internet[j], dash[1,2], nchar(data$precio_internet[j]))
          
          precio1= as.numeric(gsub("[^0-9.]", "", precio1))
          precio2= as.numeric(gsub("[^0-9.]", "", precio2))
          
          data$precio_internet[j] = as.character(max(precio1, precio2))
          data$precio_member[j]   = as.character(min(precio1, precio2))
        }
        
        #}
      }
      
      # si la palabra "normal" no aparece, asumimos que el primer precio es el de oferta y el segundo el de internet 
      # (vease base de datos) 
      
      if(str_count(data$price[j], "normal")==0){
        
        data$precio_internet[j] = substr(data$price[j], soles_dos[[i]][2,2]+1, nchar(data$price[j]))
        data$precio_member[j] = substr(data$price[j], soles_dos[[i]][1,2]+1, soles_dos[[i]][2,1]-1)
        
        if(grepl("[-]", data$precio_member[j])){
          
          dash7 = str_locate(data$precio_member[j], "[-]")
          precio1 = substr(data$precio_member[j], 1, dash7[1,1]-1)
          precio2 = substr(data$precio_member[j], dash7[1,2], nchar(data$precio_member[j]))
          
          precio1= as.numeric(gsub("[^0-9.]", "", precio1))
          precio2= as.numeric(gsub("[^0-9.]", "", precio2))
          data$precio_member[j]   = as.character(min(precio1, precio2))
        }
        
        
      }
      
    }
    
    # eliminamos las letras y las comas
    
    data$precio_internet[index_dos_precios]= as.numeric(gsub("[^0-9.]", "", data$precio_internet[index_dos_precios]))
    data$precio_normal[index_dos_precios]= as.numeric(gsub("[^0-9.]", "", data$precio_normal[index_dos_precios]))
    data$precio_member[index_dos_precios]= as.numeric(gsub("[^0-9.]", "", data$precio_member[index_dos_precios]))
    
    
    # separamos los precios de los productos con tres precios
    
    soles_tres = str_locate_all(data$price[index_tres_precios], "s/")   # lista de loc. de simbolos s/
    int_tres   = str_locate(data$price[index_tres_precios], "internet") # lista de loc. de la palabra internet
    
    for (i in 1:length(soles_tres)){   # recorre la lista de localizaciones
      
      j = index_tres_precios[i]
      
      # si la palabra "internet" se encuentra antes del tercer simbolo "s/" (siempre se cumple) asumimos: 
      # primer precio - oferta, segundo precio-internet, tercer precio-normal
      
      if( int_tres[i, 2] < soles_tres[[i]][3,1]){
        
        data$precio_member[j]   = substr(data$price[j], soles_tres[[i]][1,2]+1, soles_tres[[i]][2,1])
        data$precio_internet[j] = substr(data$price[j], soles_tres[[i]][2,2]+1, int_tres[i,1]-1)
        data$precio_normal[j]   = substr(data$price[j], soles_tres[[i]][3,2]+1, nchar(data$price[j]))
        if(grepl("-", data$precio_internet[j])){
          dash1 = str_locate(data$precio_internet[j], '-')
          precio1 = substr(data$precio_internet[j], 1, dash1[1,1]-1)
          precio2 = substr(data$precio_internet[j], dash1[1,2], nchar(data$precio_internet[j]))
          
          precio1= as.numeric(gsub("[^0-9.]", "", precio1))
          precio2= as.numeric(gsub("[^0-9.]", "", precio2))
          
          data$precio_internet[j] = as.character(min(precio1, precio2))
        }
        
      }
    }
    
    # eliminamos las letras y las comas
    
    data$precio_internet[index_tres_precios]= gsub("[^0-9.]", "", data$precio_internet[index_tres_precios])
    data$precio_normal[index_tres_precios]= gsub("[^0-9.]", "", data$precio_normal[index_tres_precios])
    data$precio_member[index_tres_precios]= gsub("[^0-9.]", "", data$precio_member[index_tres_precios])
    
    # creamos nueva base de datos con informaciones relevantes
    
    dat = select(data,categoria,subcategoria,grupo,marca,
                 descripcion,precio_normal,precio_internet,precio_member, rating)
    
    dat$categoria = as.factor(dat$categoria)
    dat$descripcion=as.character(dat$descripcion)
    dat$subcategoria=as.factor(dat$subcategoria)
    dat$grupo=as.factor(dat$grupo)
    dat$precio_internet=as.numeric(dat$precio_internet)
    dat$precio_normal=as.numeric(dat$precio_normal)
    dat$precio_member=as.numeric(dat$precio_member)
    
    "PASO 6: Exportacion de los datos"
    
    # fname_R = paste("./_R/Falabella_per_",fecha,".Rdata",sep="")
    fname_R = paste("./Base/R1/Falabella_per_",fecha,".RData",sep="")
    save(dat,file=fname_R)
    
    
  }
  fecha=fecha+1 
}

rm(list=ls())

# Termino proceso
