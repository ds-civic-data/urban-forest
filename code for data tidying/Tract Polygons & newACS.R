library(tidyverse)
library(readr)
library(readxl)
library(lubridate)
library(sp)
library(rgdal)
library(ggmap)
library(maptools)
library(raster)
library(spdep)

list.files('~/urban-forest/data-raw/', pattern='\\.shp$')
file.exists('~/urban-forest/data-raw/cb_2017_41_tract_500k.shp')

census_spatial <- readOGR(dsn=path.expand("~/urban-forest/data-raw/"), 
                          layer="cb_2017_41_tract_500k") 

newtidytract2016 <- read_csv("data/newtidytract2016.csv")

z <- newtidytract2016 %>%
  filter(`Census Tract` != "Census Tract 9800") %>%
  mutate(`% Canopy Cover` = 1 - `% Canopy Coverage`) %>%
  na.omit()

tract_stuff <- merge(census_spatial, z, by.x = "GEOID", by.y = "fips") 
tract_stuff <- subset(tract_stuff, `% Canopy Coverage` > 0)


require(RColorBrewer)
library(leaflet)

qpal<-colorQuantile("OrRd", tract_stuff@data$`% Canopy Cover`, n=9) 

leaflet(tract_stuff) %>%
  addPolygons(stroke = FALSE, fillOpacity = .8, smoothFactor = 0.2, color = ~qpal(`% Canopy Cover`)
  ) %>%
  addTiles()

list.queen<-poly2nb(tract_stuff, queen=TRUE)
W<-nb2listw(list.queen, style="W", zero.policy=TRUE)
W
plot(W,coordinates(tract_stuff))

coords<-coordinates(tract_stuff)
W_dist<-dnearneigh(coords,0,1,longlat = FALSE)

f1 <- `% Canopy Cover` ~ `% Occupied Housing Units: Owner Occupied` +
  `Median Family Income` + `% Households: Less than $40,000` +
  `% Total Population: Black or African American Alone`

nb <- poly2nb(tract_stuff)
lw <- nb2listw(nb)

m1s = lagsarlm(f1, data=tract_stuff, lw, tol.solve=1.0e-30)

summary(m1s)

a <- residuals(m1s)

grps <- 10
brks <- quantile(tract_stuff$a, 0:(grps-1)/(grps-1), na.rm=TRUE)
p <- spplot(tract_stuff, "a", at=brks, col.regions=rev(brewer.pal(grps, "RdBu")),
            col="transparent")
print( p + layer(sp.polygons(tract_stuff)) )

