library(tidyverse)
library(raster)
library(rgdal)
library(readr)

# to revise:
## proper directory paths

# requires Canopy20141.tiff in folder raster
# cb_2017_31_tract_500k shapefile in folder tracts


filename <-  "raster/Canopy20141.tif"

canopy <- raster(filename)

# convert raster into dataframe
canopyData <- as.data.frame(canopy, xy=TRUE)

# make variable easier to work with
canopyData <- canopyData %>%
  rename(canopy = Canopy20141)

# take a sample, convert NA values into 0
canopy_small <- canopyData %>%
  sample_n(100000) %>%
  mutate(canopy = ifelse(is.na(canopy), 0, canopy))

# prepare (campled) canopy dataframe to turn into SpatialPoints
canopy_xy <- canopy_small %>%
  select(lon=x, lat=y)

canopy_data <-canopy_small %>%
  select(canopy)

# take note of old projection
oldCanopyProj <- projection(canopy)

# create SpatialPoint, assign projection of original raster so we can reproject
canopyPoints <- SpatialPointsDataFrame(coords = canopy_xy, data=canopy_data, 
                                       proj4string = CRS(oldCanopyProj))

# set target projection
newproj <- "+init=epsg:4326"

# import tidy SpatialPolygons (tracts for Oregon)
tract <- readOGR(dsn = "tracts", layer = "cb_2017_41_tract_500k")

# subset to Portland tracts
tractShape <- subset(tract, COUNTYFP %in% c("005", "051"))

# set new projection
canopyPoints <- canopyPoints %>%
  spTransform(CRS(newproj))

tractShape<- tractShape %>%
  spTransform(CRS(newproj))

# for each point inside a tract, add GEOID of tract to its data
canopyPoints$tract <- over(canopyPoints, tractShape)$GEOID

# covert SpatialPoints (now with tract info) into a dataframe
canopyTract <-  canopyPoints %>%
  as.data.frame()


# for each tract, find proportion of non-zero points
tractCoverage <- canopyTract %>%
  mutate(covered = canopy > 0) %>%
  group_by(tract) %>%
  summarize(cover = mean(covered)) %>%
  # change class of tract from factor into numeric (for easy joining)
  mutate(tract = as.numeric(levels(tract)[tract])) %>%
  rename(GEOID = tract)


# tractCoverage is now able to join with tidytract by GEOID!

write_csv(tractCoverage, "tractCoverage.csv")