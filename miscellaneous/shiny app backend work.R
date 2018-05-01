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

lep_all <- read_csv('~/urban-forest/data/lep_all.csv', 
                              col_names = T)
tidytract2016_spatial <- read_csv('~/urban-forest/data/tidytract2016_spatial.csv', 
                          col_names = T)
portland <- get_map(location = c(lon = -122.66, lat = 45.531), zoom = 11, 
                    maptype = "terrain")
lep_tidytract2016 <- read_csv('~/urban-forest/data/lep_tidytract2016.csv', 
                              col_names = T)
n_portland <- get_map(location = c(lon = -122.69, lat = 45.56), zoom = 12, 
                      maptype = "terrain") 

ggmap(n_portland)

se_portland <- get_map(location = c(lon = -122.59, lat = 45.48), zoom = 12, 
                       maptype = "terrain")
ggmap(se_portland)

sw_portland <- get_map(location = c(lon = -122.73, lat = 45.485), zoom = 12, 
                       maptype = "terrain")

ggmap(sw_portland)

nw_portland <- get_map(location = c(lon = -122.745, lat = 45.54), zoom = 12, 
                       maptype = "terrain")

ggmap(nw_portland)

ne_portland <- get_map(location = c(lon = -122.59, lat = 45.55), zoom = 12, 
                       maptype = "terrain")
ggmap(ne_portland)

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

street_trees_thin <- read_csv('~/urban-forest/data/street_trees_thin.csv', 
                             col_names = T)

n_name <- street_trees_thin %>%
  distinct(Neighborhood) %>%
  dplyr::select(Neighborhood) %>%
  print()



#NE
#10 KING 
#20 IRVINGTON
#32 ALAMEDA
#23 LAURELHURST
#61 SULLIVAN'S GULCH
#26 HOLLYWOOD
#28 GRANT PARK
#45 LLOYD DISTRICT
#42 ELIOT
#14 CONCORDIA
#22 BOISE
#59 WOODLAWN
#62 HUMBOLDT
#49 SABIN
#44 PARKROSE HEIGHTS
#27 HAZELWOOD
#91 WOODLAND PARK
#72 ARGAY


#SE
#3 SELLWOOD-MORELAND  
#19 REED
#57 EASTMORELAND
#52 FOSTER-POWELL
#36 NORTH TABOR
#71 MT. TABOR
#93 ARDENWALD-JOHNSON CREEK
#2 HOSFORD-ABERNETHY  
#16 CRESTON-KENILWORTH
#38 BUCKMAN
#33 WOODSTOCK
#50 BRENTWOOD-DARLINGTON
#53 BROOKLYN
#21 MONTAVILLA
#47 MT. SCOTT-ARLETA
#41 KERNS
#1 POWELLHURST-GILBERT
#43 PLEASANT VALLEY
#68 MILL PARK
#35 LENTS
#48 SOUTH TABOR
#39 CENTENNIAL
#6 RICHMOND           
#5 SUNNYSIDE          



#NW
#69 OLD TOWN/CHINATOWN
#8 PEARL              
#9 NORTHWEST DISTRICT 
#75 NORTHWEST HEIGHTS
#78 NORTHWEST INDUSTRIAL
#79 HILLSIDE
#82 LINNTON


#SW
#74 SYLVAN HIGHLANDS
#18 DOWNTOWN
#80 ARLINGTON HEIGHTS
#25 GOOSE HOLLOW
#4 HILLSDALE          
#81 ARNOLD CREEK
#84 ASH CREEK
#40 MULTNOMAH
#46 COLLINS VIEW
#51 HOMESTEAD
#65 SOUTH BURLINGAME
#66 SOUTHWEST HILLS
#13 SOUTH PORTLAND
#88 CRESTWOOD
#89 BRIDLEMILE
#86 FAR SOUTHWEST
#92 MARSHALL PARK
#73 WEST PORTLAND PARK
#90 MAPLEWOOD
#70 HAYHURST


#N
#11 KENTON
#77 EAST COLUMBIA
#85 BRIDGETON
#87 HAYDEN ISLAND
#67 UNIVERSITY PARK
#63 ARBOR LODGE
#55 PIEDMONT
#60 PORTSMOUTH
#17 OVERLOOK
#24 ST. JOHNS
#29 CATHEDRAL PARK


#7 ROSE CITY PARK     
#12 RUSSELL
#15 ROSEWAY
#30 BEAUMONT-WILSHIRE
#31 SUMNER
#34 PARKROSE
#37 CULLY
#54 GLENFAIR
#56 WILKES
#58 MADISON SOUTH
#64 VERNON
#76 PDX
#83 SUNDERLAND

