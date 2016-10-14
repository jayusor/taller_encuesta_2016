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
colnames(meta)<-c("date", 
                  "estad", "bibli", "parki", "comid", 
                  "aulas", "depor", "profe", "segur", 
                  "extra", "resid", "satis", "recom",
                  "nombre", "domicilio", "email")
str(meta)


#' Inventarnos unos cuantos registros de prueba
#' 200 registros con nombre Maria y 200 registros con nombre Antonio
#+ eval=FALSE
size=20
data.frame(
  date=Sys.time() %>% as.character,
  estad=sample(x=0:10, size, replace=TRUE, prob=seq(from=100, to=10, length.out = 11)),
  bibli=sample(x=0:10, size, replace=TRUE, prob=seq(from=100, to=10, length.out = 11)),
  parki=sample(x=0:10, size, replace=TRUE, prob=seq(from=100, to=10, length.out = 11)),
  comid=sample(x=0:10, size, replace=TRUE, prob=seq(from=100, to=10, length.out = 11)),
  aulas=sample(x=0:10, size, replace=TRUE, prob=seq(from=100, to=10, length.out = 11)),
  depor=sample(x=0:10, size, replace=TRUE, prob=seq(from=100, to=10, length.out = 11)),
  profe=sample(x=0:10, size, replace=TRUE, prob=seq(from=100, to=10, length.out = 11)),
  segur=sample(x=0:10, size, replace=TRUE, prob=seq(from=100, to=10, length.out = 11)),
  extra=sample(x=0:10, size, replace=TRUE, prob=seq(from=100, to=10, length.out = 11)),
  resid=sample(x=0:10, size, replace=TRUE, prob=seq(from=100, to=10, length.out = 11)),
  satis=sample(x=0:10, size, replace=TRUE, prob=seq(from=100, to=10, length.out = 11)),
  recom=sample(x=0:10, size, replace=TRUE, prob=seq(from=100, to=10, length.out = 11)),
  nombre="Patricia",
  domicilio="Calle Mayor, Madrid",
  email="aschinchon@gmail.com"
) %>% rbind(meta) -> meta

data.frame(
  date=Sys.time() %>% as.character,
  estad=sample(x=0:10, size, replace=TRUE, prob=seq(from=10, to=100, length.out = 11)),
  bibli=sample(x=0:10, size, replace=TRUE, prob=seq(from=10, to=100, length.out = 11)),
  parki=sample(x=0:10, size, replace=TRUE, prob=seq(from=10, to=100, length.out = 11)),
  comid=sample(x=0:10, size, replace=TRUE, prob=seq(from=10, to=100, length.out = 11)),
  aulas=sample(x=0:10, size, replace=TRUE, prob=seq(from=10, to=100, length.out = 11)),
  depor=sample(x=0:10, size, replace=TRUE, prob=seq(from=10, to=100, length.out = 11)),
  profe=sample(x=0:10, size, replace=TRUE, prob=seq(from=10, to=100, length.out = 11)),
  segur=sample(x=0:10, size, replace=TRUE, prob=seq(from=10, to=100, length.out = 11)),
  extra=sample(x=0:10, size, replace=TRUE, prob=seq(from=10, to=100, length.out = 11)),
  resid=sample(x=0:10, size, replace=TRUE, prob=seq(from=10, to=100, length.out = 11)),
  satis=sample(x=0:10, size, replace=TRUE, prob=seq(from=10, to=100, length.out = 11)),
  recom=sample(x=0:10, size, replace=TRUE, prob=seq(from=10, to=100, length.out = 11)),
  nombre="Antonio",
  domicilio="Calle Mayor, Madrid",
  email="aschinchon@gmail.com"
) %>% rbind(meta) -> meta


#' Seleccionar los que dicen que el estado del campus es 3 o menos
meta %>% filter(estad<=3)

#' Summary del dataset
summary(meta)

#' Histograma de una respuesta
hist(meta$comid, main = "Valoracion de las comidas")

#' ## Creamos algunas variables nuevas
#' Promotor/Detractor
meta %>% 
  mutate(nps=ifelse(recom<3, "Detractor", ifelse(recom>7, "Promotor", "Neutro")))-> meta

#' Genero (M/F)
library(genderizeR)
unique(meta$nombre) %>% 
  findGivenNames(textPrepare = FALSE) -> sexo

#' Incorporamos la variable gender 
meta %>% left_join(sexo, by=c("nombre"="name")) -> meta

#' Media agrupada de una variable
meta %>% group_by(gender) %>% summarize(puntuacion_comidas=mean(extra))

#' Geolocalizacion
library(ggmap)
meta %>% 
  select(domicilio) %>% 
  lapply(function(x){geocode(as.character(x), output="latlon")})  %>% 
  as.data.frame %>% 
  cbind(meta) -> meta

#' ## Algunos graficos interactivos (Leaflet, highcharter, googlevis)
#' Mapa con la localizacion de los encuestados
library(leaflet)
leaflet(meta) %>%
  addTiles() %>%
  addCircleMarkers(lng = ~domicilio.lon, 
                   lat = ~domicilio.lat, 
                   radius = 5, 
                   color = "red",
                   stroke=FALSE,
                   fillOpacity = 0.8)->mapa

#' Highchart
library(highcharter)
meta %>% group_by(gender) %>% 
  summarize(estad=mean(estad),
            bibli=mean(bibli),
            parki=mean(parki), 
            comid=mean(comid), 
            aulas=mean(aulas), 
            depor=mean(depor), 
            profe=mean(profe), 
            segur=mean(segur), 
            extra=mean(extra), 
            resid=mean(resid), 
            satis=mean(satis), 
            recom=mean(recom))->medias

medias %>% 
  filter(gender=="female") %>% 
  select(-gender) %>% 
  as.numeric->chicas

medias %>% 
  filter(gender=="male") %>% 
  select(-gender) %>% 
  as.numeric->chicos


highchart() %>% 
  hc_chart(polar = TRUE, type = "line") %>% 
  hc_title(text = "Valoraciones según sexo") %>% 
  hc_xAxis(categories = c("estad", "bibli", "parki", "comid", 
                          "aulas", "depor", "profe", "segur", 
                          "extra", "resid", "satis", "recom"),
           tickmarkPlacement = 'on',
           lineWidth = 0) %>% 
  hc_yAxis(gridLineInterpolation = 'polygon',
           lineWidth = 0,
           min = 0) %>% 
  hc_series(
    list(
      name = "Chicos",
      data = chicos,
      pointPlacement = 'on'
    ),
    list(
      name = "Chicas",
      data = chicas,
      pointPlacement = 'on'
    )
  )->spider

#' ## arbol de decision para separar promotores de detractores 
library(party)
meta %>% 
  filter(nps %in% c("Detractor", "Promotor")) %>%
  mutate(nps=ifelse(nps=="Promotor", 1, 0)) %>% 
  ctree(nps~estad+bibli+parki+comid+aulas+depor+profe+
            segur+extra+resid+satis, data=.)->tree
plot(tree, main="Classification Tree Promotores/Detractores")
