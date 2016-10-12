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

#' Inventarnos unos cuantos registros de prueba
#' 200 registros con nombre Maria y 200 registros con nombre Antonio
#+ eval=FALSE
size=50
data.frame(
date=Sys.time() %>% as.character,
estado=sample(x=0:10, size, replace=TRUE, prob=seq(from=100, to=10, length.out = 11)),
biblioteca=sample(x=0:10, size, replace=TRUE, prob=seq(from=100, to=10, length.out = 11)),
accesibilidad_parking=sample(x=0:10, size, replace=TRUE, prob=seq(from=100, to=10, length.out = 11)),
comidas=sample(x=0:10, size, replace=TRUE, prob=seq(from=100, to=10, length.out = 11)),
inst_aulas=sample(x=0:10, size, replace=TRUE, prob=seq(from=100, to=10, length.out = 11)),
inst_deporte=sample(x=0:10, size, replace=TRUE, prob=seq(from=100, to=10, length.out = 11)),
accesibilidad_profes=sample(x=0:10, size, replace=TRUE, prob=seq(from=100, to=10, length.out = 11)),
seguridad=sample(x=0:10, size, replace=TRUE, prob=seq(from=100, to=10, length.out = 11)), 
extraescolares=sample(x=0:10, size, replace=TRUE, prob=seq(from=100, to=10, length.out = 11)),
residencias=sample(x=0:10, size, replace=TRUE, prob=seq(from=100, to=10, length.out = 11)),
satisfaccion=sample(x=0:10, size, replace=TRUE, prob=seq(from=100, to=10, length.out = 11)),
recomendacion=sample(x=0:10, size, replace=TRUE, prob=seq(from=100, to=10, length.out = 11)),
nombre="Patricia",
domicilio="Calle Mayor, Madrid",
email="aschinchon@gmail.com"
) %>% rbind(meta) -> meta

data.frame(
  date=Sys.time() %>% as.character,
  estado=sample(x=0:10, size, replace=TRUE, prob=seq(from=10, to=100, length.out = 11)),
  biblioteca=sample(x=0:10, size, replace=TRUE, prob=seq(from=10, to=100, length.out = 11)),
  accesibilidad_parking=sample(x=0:10, size, replace=TRUE, prob=seq(from=10, to=100, length.out = 11)),
  comidas=sample(x=0:10, size, replace=TRUE, prob=seq(from=10, to=100, length.out = 11)),
  inst_aulas=sample(x=0:10, size, replace=TRUE, prob=seq(from=10, to=100, length.out = 11)),
  inst_deporte=sample(x=0:10, size, replace=TRUE, prob=seq(from=10, to=100, length.out = 11)),
  accesibilidad_profes=sample(x=0:10, size, replace=TRUE, prob=seq(from=100, to=10, length.out = 11)),
  seguridad=sample(x=0:10, size, replace=TRUE, prob=seq(from=10, to=100, length.out = 11)), 
  extraescolares=sample(x=0:10, size, replace=TRUE, prob=seq(from=10, to=100, length.out = 11)),
  residencias=sample(x=0:10, size, replace=TRUE, prob=seq(from=10, to=100, length.out = 11)),
  satisfaccion=sample(x=0:10, size, replace=TRUE, prob=seq(from=10, to=100, length.out = 11)),
  recomendacion=sample(x=0:10, size, replace=TRUE, prob=seq(from=10, to=100, length.out = 11)),
  nombre="Antonio",
  domicilio="Calle Mayor, Madrid",
  email="aschinchon@gmail.com"
) %>% rbind(meta) -> meta


#' Seleccionar los que dicen que el estado del campus es 3 o menos
meta %>% filter(estado<=3)

#' Summary del dataset
summary(meta)

#' Histograma de una respuesta
hist(meta$comidas, main = "Valoracion de las comidas")

#' ## Creamos algunas variables nuevas
#' Promotor/Detractor
meta %>% mutate(nps=ifelse(recomendacion<3, "Detractor", ifelse(recomendacion>7, "Promotor", "Neutro")))-> meta

#' Genero (M/F)
library(genderizeR)
unique(meta$nombre) %>% findGivenNames(textPrepare = FALSE) -> sexo

#' Incorporamos la variable gender 
meta %>% left_join(sexo, by=c("nombre"="name")) -> meta

#' Media agrupada de una variable
meta %>% group_by(gender) %>% summarize(puntuacion_comidas=mean(extraescolares))

#' Geolocalizacion
library(ggmap)
lapply(as.character(meta$domicilio), geocode)->loc

as.data.frame(loc)
meta %>% cbind(loc)->meta


#' ## Algunos graficos interactivos (Leaflet, highcharter, googlevis)
#' 


#' ## Un arbol
#' 