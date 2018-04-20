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
file.exists('~/urban-forest/data-raw/Limited_English_Proficiency.shp')

lep <- readOGR(dsn=path.expand("~/urban-forest/data-raw/"), 
                         layer="Limited_English_Proficiency")
lep_data <- lep@data 
lep_data$OBJECTID <- as.character(lep_data$OBJECTID)

plot(lep)

lep2 <- fortify(lep)

lep_all <- full_join(lep2, lep_data, by = c('id' = 'OBJECTID'))

write_csv(lep2, "lep2.csv")
write_csv(lep_all, "lep_all.csv")
