library(tidyverse)
library(readr)
library(readxl)
library(lubridate)

set.seed(666)

street_trees <- read_excel("~/urban-forest/data-raw/Street_Trees.xlsm", col_names = T) %>%
  sample_n(size = 10000, replace = F) 

species_tree <- street_trees %>%
  select(Species, Scientific, Family, Genus, Common) %>%
  unique() %>%
  print()


  