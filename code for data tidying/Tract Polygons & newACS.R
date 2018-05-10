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

tract_stuff <- merge(census_spatial, newtidytract2016, by.x = "GEOID", by.y = "fips") 
tract_stuff <- subset(tract_stuff, `Total Population` > 0)

require(RColorBrewer)
library(leaflet)

qpal<-colorQuantile("OrRd", tract_stuff@data$`% Canopy Coverage`, n=9) 

leaflet(tract_stuff) %>%
  addPolygons(stroke = FALSE, fillOpacity = .8, smoothFactor = 0.2, color = ~qpal(`% Canopy Coverage`)
  ) %>%
  addTiles()

list.queen<-poly2nb(tract_stuff, queen=TRUE)
W<-nb2listw(list.queen, style="W", zero.policy=TRUE)
W
