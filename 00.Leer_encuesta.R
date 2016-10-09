rm(list = ls());gc()


library(tidyverse)
library(googlesheets)


quiero <- "Taller Europea (respuestas)"


gs_ls() %>% filter(sheet_title==quiero) %>% str




