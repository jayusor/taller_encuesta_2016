#' ---
#' title: "Lectura Encuesta"
#' author: Lenguajes para el Análisis de Datos
#' date: "Noviembre 2016"
#' mode: standalone # {selfcontained,standalone, draft}
#' output:
#'  html_document:
#'    theme: united
#'    css: ./img/base.css
#' ---


#' ## Introducción
#' 
#' Vamos a proceder a leer los resultados de la encuesta de Google Forms
#' 


rm(list = ls());gc()

library(tidyverse)
library(googlesheets)

#' Buscar por hojas que contengan "Europea"
(hojas <- gs_ls("Europea")) %>% str

#' Guardo la key de la hoja
key_hoja <- hojas %>% select(sheet_key) %>% .[[1]]

#' Defino `wb` que hace referencia a la hoja que queremos
wb <- gs_key(key_hoja)
wb

#' Abrir en el navegador
#+ eval=FALSE
wb %>% gs_browse()

#' Leer la hoja 
meta <- wb %>% gs_read()
class(meta)

#' ## Algunas operaciones sencillas con los datos
#' Sacar unos cuantos registros
head(meta)
str(meta)

#' Renombrar campos para facilitar manejo
colnames(meta) 
colnames(meta)<-c("date", "estado", "biblioteca", "accesibilidad_parking", "comidas", 
                  "inst_aulas", "inst_deporte", "accesibilidad_profes", "seguridad", 
                  "extraescolares", "residencias", "satisfaccion", "recomendacion",
                  "nombre", "domicilio", "email")
str(meta)

#' Seleccionar los que dicen que el estado del campus es 3 o menos
meta %>% filter(estado<=3)

#' Summary del dataset
summary(meta)

#' Histograma de una respuesta
hist(meta$comidas, breaks=0:10, main = "Valoracion de las comidas")

#' x-y plot
plot(x = meta$inst_deporte, 
     y = meta$inst_aulas, 
     type = 'p', 
     main = "Valoracion de las instalaciones")

#' ## Creamos algunas variables nuevas
#' Promotor/Detractor
meta %>% mutate(nps=ifelse(recomendacion<3, "Detractor", ifelse(recomendacion>7, "Promotor", "Neutro")))

#' Genero (M/F)
library(genderizeR)
meta %>% 
  select(nombre) %>% 
  findGivenNames() -> sexo
meta %>% left_join(sexo, by=c("nombre"="name")) -> meta

#' Geolocalizacion
library(ggmap)
meta %>% 
  select(domicilio) %>% 
  lapply(geocode)->loc
meta %>% cbind(loc)->meta
