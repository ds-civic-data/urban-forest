library(tidyverse)
library(spdep)

tidytract2016 <- read_csv("~/urban-forest/data/tidytract2016.csv",
                          col_names = T)
street_trees_all <- read_csv('~/urban-forest/data/street_trees_all.csv', 
                             col_names = T)
tidytract2016_spatial <- read_csv('~/urban-forest/data/tidytract2016_spatial.csv',
                                  col_names = T)
centroids_tt <- read_csv('~/urban-forest/data/centroids_tt.csv', 
                         col_names = T)
tidytract2016_sp_cent <- read_csv('~/urban-forest/data/tidytract2016_sp_cent.csv',
                                  col_names = T)

m1 <- lm(population_density ~ white + black + med_family_income + dist,
         data = tidytract2016_sp_cent)
summary(m1)

m2 <- lm(population_density ~ white + black + med_family_income,
         data = tidytract2016_sp_cent)
summary(m2)

m3 <- lm(population_density ~ white + black + med_family_income + no_commute + below_hs + dist,
         data = tidytract2016_sp_cent)
summary(m3)

tscc <- read_csv('~/urban-forest/data/tscc.csv', col_names = T)

# tscc <- tscc %>%
  

m4 <- lm(sumOverCount ~ `Population Density (Per Sq. Mile)` + 
           `% Total Population: White Alone` +
           `% Population: Master's Degree`+
           `Median Family Income` +
           `Gini Index` +
           `% Workers with No Commute` +
           percent_water +
           dist,
         tscc)
summary(m4)

m5 <- lm(sumOverCount ~ `Median Family Income` +
           `% Workers with No Commute` +
           dist +
           percent_water,
         tscc)
summary(m5)

m6 <- lm(sumOverCount ~ `Median Family Income`, tscc)
summary(m6)

m7 <- lm(sumOverCount ~ `% Workers with No Commute`,
         tscc)
summary(m7)

m8 <- lm(sumOverCount ~ `Median Family Income` +
           percent_water +
           dist, tscc)
summary(m8)

# depreciated
# write_csv(tt_sp_cent_cov, '~/urban-forest/data/tidytract2016_sp_cent.csv')

m9 <- lm(sumOverCount ~ `Median Family Income` +
          `% Civilian Population in Labor Force 16 Years and Over: Unemployed` +
          `% Total Population: Black or African American Alone` +
          `Population Density (Per Sq. Mile)` +
           `Gini Index` +
           `% Workers with No Commute` +
          `% Population: Bachelor's Degree` +
          `% Occupied Housing Units: Owner Occupied` +
           percent_water +
           dist,
         tscc)
summary(m9)

m10 <- lm(sumOverCount ~ `Median Family Income` +
            `% Civilian Population in Labor Force 16 Years and Over: Unemployed` +
            `% Total Population: Black or African American Alone` +
            `Population Density (Per Sq. Mile)` +
            `Gini Index` +
            `% Workers with Over 40 Minute Commute` +
            `% Population: Bachelor's Degree` +
            `% Occupied Housing Units: Owner Occupied` +
            percent_water +
            dist,
          tscc)
summary(m10)

tscc_water <- tscc %>%
  mutate(coverage_land = sumOverCount / (1 - percent_water))

m10 <- lm(coverage_land ~ `Median Family Income` +
            `% Civilian Population in Labor Force 16 Years and Over: Unemployed` +
            `% Total Population: Black or African American Alone` +
            `Population Density (Per Sq. Mile)` +
            `Gini Index` +
            `% Workers with Over 40 Minute Commute` +
            `% Population: Bachelor's Degree` +
            `% Occupied Housing Units: Owner Occupied` +
            dist,
          tscc_water)
summary(m10)

m12 <- lm(sumOverCount ~ `Median Family Income` +
            `Population Density (Per Sq. Mile)` +
            `Gini Index` +
            `% Population: Bachelor's Degree` +
            `% Occupied Housing Units: Owner Occupied` +
            percent_water +
            dist,
          tscc)
summary(m12)


















library(tidyverse)
library(readr)
library(readxl)
library(lubridate)
library(sp)
library(rgdal)
library(ggmap)
library(maptools)
library(raster)
library(spData)
library(spdep)
library(graphics)
library(grDevices)
library(readr)
library(rgdal)


list.files('~/urban-forest/data-raw/', pattern='\\.shp$')
file.exists('~/urban-forest/data-raw/cb_2017_41_tract_500k.shp')

census_spatial <- readOGR(dsn=path.expand("~/urban-forest/data-raw/"), 
                          layer="cb_2017_41_tract_500k") 

z <- newtidytract2016 %>%
  filter(`% Canopy Coverage` >= 0.4)

tract_stuff <- merge(census_spatial, z, by.x = "GEOID", by.y = "fips") 
tract_stuff <- subset(tract_stuff, `% Canopy Coverage` > 0)

library(leaflet)

qpal<-colorQuantile("OrRd", tract_stuff@data$`% Canopy Coverage`, n=9) 
list.queen<-poly2nb(tract_stuff, queen=TRUE)
W<-nb2listw(list.queen, style="W", zero.policy=TRUE)
coords<-coordinates(tract_stuff)
W_dist<-dnearneigh(coords,0,1,longlat = FALSE)

f1 <- `% Canopy Coverage` ~ `Median Gross Rent` +
  `Median Family Income` +
  `Gini Index` +
  `Area (Land, in Sq. Miles)` +
  `% Households: Less than $40,000` 

nb <- poly2nb(tract_stuff)
lw <- nb2listw(nb)

m1s = lagsarlm(f1, data=tract_stuff, lw, tol.solve=1.0e-30)

summary(m1s)
