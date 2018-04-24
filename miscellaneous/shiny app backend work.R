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
portland <- get_map(location = c(lon = -122.66, lat = 45.531), zoom = 11, 
                    maptype = "terrain")
lep_tidytract2016 <- read_csv('~/urban-forest/data/lep_tidytract2016.csv', 
                              col_names = T)

lep_all %>%
  filter(group >= 3.1) %>%
  dplyr::select(long, lat, group) %>%
  print()

tidytract2016_spatial %>%
  dplyr::select(long, lat, group) %>%
  print()


ggmap(portland) +
  geom_polygon(data = tidytract2016_spatial, 
         aes(x=long, y=lat, group=group, fill=hhs_200k_more),
         #input$fill_opts), 
         alpha = 0.6) 

sum(is.na(lep_tidytract2016$long.x))
# 0 are NA

sum(is.na(lep_tidytract2016$long.y))
# 5221 are NA

sum(is.na(lep_tidytract2016$group))
# 0 are NA