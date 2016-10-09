rm(list = ls());gc()

library(tidyverse)
library(googlesheets)

## Buscar por hojas que contengan "Europea"
(hojas <- gs_ls("Europea")) %>% str

## Guardo la key de la hoja
key_hoja <- hojas %>% select(sheet_key) %>% .[[1]]

## Defino `wb` que hace referencia a la hoja que queremos
wb <- gs_key(key_hoja)
wb

## Abrir en el navegador
# wb %>% gs_browse()

## Leer la hoja 
meta <- wb %>% gs_read()
class(meta)

meta
