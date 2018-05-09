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
file.exists('~/urban-forest/data-raw/Neighborhood_Boundaries.shp')

neighborhoods <- readOGR(dsn=path.expand("~/urban-forest/data-raw/"), 
                         layer="Neighborhood_Boundaries")
n_data <- neighborhoods@data 
n_data$OBJECTID <- as.character(n_data$OBJECTID)

plot(neighborhoods)

neighborhoods2 <- fortify(neighborhoods)

neighborhoods_all <- full_join(neighborhoods2, n_data, by = c('id' = 'OBJECTID'))

write_csv(neighborhoods2, "neighborhoods2.csv")
write_csv(neighborhoods_all, "neighborhoods_all.csv")
