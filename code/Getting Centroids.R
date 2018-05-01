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
library(rgeos)

install.packages('PBSmapping')
library(PBSmapping)

lep_all <- read_csv('~/urban-forest/data/lep_all.csv', 
                    col_names = T)
tidytract2016_spatial <- read_csv('~/urban-forest/data/tidytract2016_spatial.csv', 
                                  col_names = T)
portland <- get_map(location = c(lon = -122.66, lat = 45.531), zoom = 11, 
                    maptype = "terrain")
lep_tidytract2016 <- read_csv('~/urban-forest/data/lep_tidytract2016.csv', 
                              col_names = T)

install.packages("GISTools")
library(GISTools)

tt_coords <- tidytract2016_spatial %>%
  dplyr::select(long, lat, order, group, hhs_200k_more) %>%
  rename('X' = long, 'Y' = lat, 'PID' = group, 'POS' = order) %>%
  filter(!is.na(hhs_200k_more)) 

centroids_tt <- calcCentroid(as.PolySet(tt_coords))

ggmap(portland) +
  geom_polygon(data = tidytract2016_spatial, 
               aes(x=long, y=lat, group=group, fill=hhs_200k_more), alpha = 0.6) +
  #geom_polygon(data = neighborhoods_all, aes(x=long, y=lat, group=group), 
  #             col = 'dark grey', 
   #            fill = 'transparent') +
  geom_point(data = centroids_tt,
             aes(x=X, y=Y, group = PID),
             size = 0.8)  +
  scale_fill_gradientn(colours = heat.colors(7), na.value = 'transparent') +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
  theme_bw()

write_csv(centroids_tt, "centroids_tt.csv")
