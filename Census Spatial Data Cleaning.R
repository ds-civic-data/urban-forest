library(tidyverse)
library(readr)
library(readxl)
library(lubridate)
library(sp)
library(rgdal)
library(ggmap)
library(maptools)
library(raster)

list.files('~/urban-forest/data-raw/', pattern='\\.shp$')
file.exists('~/urban-forest/data-raw/cb_2017_41_tract_500k.shp')

census_spatial <- readOGR(dsn=path.expand("~/urban-forest/data-raw/"), 
               layer="cb_2017_41_tract_500k") 

census_spatial_data <- census_spatial@data 
census_spatial_data <- cbind(census_spatial_data, id = 0:((length(census_spatial))-1)) 
census_spatial_data$id <- as.character(census_spatial_data$id)

census_spatial2 <- fortify(census_spatial) 

census_spatial_all <- full_join(census_spatial2, census_spatial_data,
                                by = c('id' = 'id'))

census_spatial_all <-
  filter(census_spatial_all, COUNTYFP == '005' | COUNTYFP == '051')

write_csv(census_spatial_all, "census_spatial_all.csv")
