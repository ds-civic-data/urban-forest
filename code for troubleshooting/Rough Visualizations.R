library(tidyverse)
library(readr)
library(readxl)
library(lubridate)
library(sp)
library(rgdal)
library(ggmap)
library(maptools)
library(raster)

lep_all <- read_csv('~/urban-forest/data/lep_all.csv', col_names = T)
street_trees_thin <- read_csv('~/urban-forest/data/street_trees_thin.csv', col_names = T)
neighborhoods_all <- read_csv('~/urban-forest/data/neighborhoods_all.csv', col_names = T)
census_spatial_all <- read_csv('~/urban-forest/data/census_spatial_all.csv', col_names = T)
tidytract2106_spatial <- read_csv('~/urban-forest/data/tidytract2016_spatial.csv', 
                                  col_names = T)
portland <- get_map(location = c(lon = -122.66, lat = 45.531), zoom = 11, 
                    maptype = "terrain")


ggmap(portland, base_layer = ggplot(neighborhoods_all)) +
  geom_point(data = street_trees_thin, aes(x=X, y=Y, col=Size), alpha = 0.09) 
  geom_polygon(data = neighborhoods_all, aes(x=long, y=lat, group=group), col = 'black',
               fill = 'transparent') +
  labs(title = "Street Trees in Portland") +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
  theme_bw()

ggmap(portland, base_layer = ggplot(lep_all)) +
  geom_polygon(data = lep_all, aes(x=long, y=lat, group = group, fill = Total_Pop_), 
               alpha = 0.7) +
  scale_color_gradientn(colours = terrain.colors(7)) +
  geom_polygon(data = neighborhoods_all, aes(x=long, y=lat, group=group), col = 'black',
               fill = 'transparent', lwd = 0.3) +
  labs(title = "Limited English Proficiency Population in Portland") +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
  theme_bw()

ggmap(portland, base_layer = ggplot(lep_all)) +
  geom_polygon(data = lep_all, aes(x=long, y=lat, group = group, fill = Russian), 
               alpha = 0.7) +
  scale_color_gradientn(colours = terrain.colors(7)) +
  geom_polygon(data = neighborhoods_all, aes(x=long, y=lat, group=group), col = 'black',
               fill = 'transparent', lwd = 0.3) +
  labs(title = "Russian-Only Speaking Population in Portland") +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
  theme_bw()

ggmap(portland, base_layer = ggplot(tidytract2106_spatial)) +
  geom_polygon(data = tidytract2106_spatial, 
               aes(x=long, y=lat, group = group, fill = total_population), 
               alpha = 0.7) +
  scale_fill_gradient(low = "yellow", high = "red") +
  geom_polygon(data = neighborhoods_all, aes(x=long, y=lat, group=group), 
               col = 'black',
               fill = 'transparent', lwd = 0.3) +
  labs(title = "Population by Census Tract in Portland") +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
  theme_bw()

ggmap(portland, base_layer = ggplot(neighborhoods_all)) +
  geom_polygon(data = tidytract2106_spatial, 
               aes(x=long, y=lat, group=group, fill=bachelors_degree),
               alpha = 0.6) +
  geom_polygon(data = neighborhoods_all, aes(x=long, y=lat, group=group), 
               col = 'black', fill = 'transparent') +
  geom_point(data = street_trees_thin, aes(x=X, y=Y), col = 'dark green',
             alpha = 0.05, size = 0.8) +
  scale_fill_gradientn(colours = terrain.colors(7), na.value = 'transparent') +
  labs(title = "Street Trees and Bachelors Degrees in Portland") +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
  theme_bw()

