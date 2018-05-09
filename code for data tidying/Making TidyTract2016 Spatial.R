library(tidyverse)
library(readr)
library(readxl)
library(lubridate)
library(sp)
library(rgdal)
library(ggmap)
library(maptools)
library(raster)

census_spatial_all <- read_csv('~/urban-forest/data/census_spatial_all.csv', col_names = T)
tidytract2106 <- read_csv('~/urban-forest/data/tidytract2016.csv', col_names = T)

tidytract2016_spatial <- full_join(census_spatial_all, tidytract2106, 
                                   by = c('GEOID' = 'fips'))

write_csv(tidytract2016_spatial, '~/urban-forest/data/tidytract2016_spatial.csv')
