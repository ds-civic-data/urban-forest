library(shiny)
library(tidyverse)
library(readr)
library(readxl)
library(lubridate)
library(sp)
library(rgdal)
library(ggmap)
library(maptools)
library(raster)

lep_all <- read_csv('~/urban-forest/data/lep_all.csv', 
                              col_names = T)
tidytract2016_spatial <- read_csv('~/urban-forest/data/tidytract2016_spatial.csv', 
                          col_names = T)

lep_all %>%
  filter(group >= 3.1) %>%
  dplyr::select(long, lat, group) %>%
  print()

tidytract2016_spatial %>%
  dplyr::select(long, lat, group) %>%
  print()
