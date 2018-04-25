library(tidyverse)
library(readr)
library(readxl)
library(lubridate)
library(sp)
library(ggmap)

set.seed(666)

street_trees <- read_excel("~/urban-forest/data-raw/Street_Trees.xlsm", 
                           col_names = T) %>%
  sample_n(size = 10000, replace = F) 

species_tree <- street_trees %>%
  select(Species, Scientific, Family, Genus, Common) %>%
  unique() %>%
  print()

# coordinates(street_trees) <- ~X+Y

portland <- ggmap(get_map(c(lon = -122.62, lat = 45.53), zoom = 11, 
                          maptype = "roadmap", color = "bw"))

portland + 
  geom_point(aes(x=X, y=Y, col = Size), alpha = 0.4, data = street_trees)

sort(species_tree$Common, decreasing = F)

class(coordinates(street_trees))
class(data_frame(coordinates(street_trees)))

street_trees$Site_Size <- factor(street_trees$Site_Size, 
                                 levels = c("Small", "Medium", "Large"))

street_trees_thin <- street_trees %>%
  select(X, Y, Date_Inventoried, Species, Condition, Neighborhood, 
         Family, Genus, Common, Size) %>%
  filter(!is.na(X) & !is.na(Y) & !is.na(Size) & !is.na(Neighborhood) & 
           Common != "unknown") %>%
  as_date(street_trees$Date_Inventoried)

street_trees_thin$Date_Inventoried <- as_date(street_trees_thin$Date_Inventoried)
head(street_trees_thin)
write_csv(street_trees_thin, "~/urban-forest/data/street_trees_thin.csv")

street_trees_all <- read_excel("~/urban-forest/data-raw/Street_Trees.xlsm", 
                           col_names = T)
#dummy_tree <- c(-100, 44, NA, 'DD', 'Good', 'x', 'Dummy', 'Dummy', 'dummy', 'D')
#street_trees_all <- rbind(street_trees_all, dummy_tree, dummy_tree, dummy_tree, 
#                          dummy_tree, dummy_tree, dummy_tree, dummy_tree, dummy_tree, 
#                          dummy_tree, dummy_tree, dummy_tree, dummy_tree, dummy_tree, 
#                          dummy_tree, dummy_tree, dummy_tree, dummy_tree, dummy_tree)
street_trees_all <- street_trees_all %>%
  dplyr::select(X, Y, Date_Inventoried, Species, Condition, Neighborhood, 
         Family, Genus, Common, Size) %>%
  filter(!is.na(X) & !is.na(Y) & !is.na(Size) & !is.na(Neighborhood) & 
           Common != "unknown") %>%
  mutate(`Canopy Coverage` = ifelse(Size == 'S', 0.01, 
                               ifelse(Size == 'M', 0.02,
                                      ifelse(Size == 'L', 0.03, 5))))

write_csv(street_trees_all, "~/urban-forest/data/street_trees_all.csv")
