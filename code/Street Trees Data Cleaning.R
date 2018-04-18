library(tidyverse)
library(readr)
library(readxl)
library(lubridate)
library(sp)
library(ggmap)

set.seed(666)

street_trees <- read_excel("~/urban-forest/data-raw/Street_Trees.xlsm", col_names = T) %>%
  sample_n(size = 10000, replace = F) 

species_tree <- street_trees %>%
  select(Species, Scientific, Family, Genus, Common) %>%
  unique() %>%
  print()

# coordinates(street_trees) <- ~X+Y

portland <- ggmap(get_map(c(lon = -122.62, lat = 45.53), zoom = 11, maptype = "roadmap", color = "bw"))

portland + 
  geom_point(aes(x=X, y=Y, col = Size), alpha = 0.4, data = street_trees)
