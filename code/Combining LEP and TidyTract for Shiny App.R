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
tidytract2106_spatial <- read_csv('~/urban-forest/data/tidytract2016_spatial.csv', 
                                  col_names = T)

lep_tidytract2016 <- left_join(tidytract2106_spatial, lep_all, 
                               by = c('TRACTCE' = 'TRACT')) %>%
  dplyr::select(-STATE, -COUNTY, -STATEFP, -COUNTYFP, -AFFGEOID, -OBJECTID, 
                -GEO_ID, -Geography, -Shape_Leng, -Shape_Area, -hole.x, -piece.x,
                -ALAND, -AWATER, -hole.y, -LSAD.x, -LSAD.y, -Id2, -piece.y, -id.y, 
                -order.x, -id.x, -qualifying_name, -Other_Slav, -Other_Indi,
                -Other_Indo, -Other_Asia, -Other_Paci) %>%
  rename('group' = TRACTCE)


write_csv(lep_tidytract2016, "~/urban-forest/data/lep_tidytract2016.csv")