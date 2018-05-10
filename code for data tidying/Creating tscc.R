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
newtidytract2016 <- read_csv('~/urban-forest/data/newtidytract2016.csv',
                             col_names = T)

centroids_tt <- centroids_tt %>%
  rename(cent_x = 'X',
         cent_y = 'Y')

distance <- dist(centroids_tt[,2:3], method = 'euclidean', diag = F)
dist <- as.matrix(distance)
dist <- dist[1,]
dist <- as.data.frame(dist)

centroids_tt <- cbind(centroids_tt, dist)

tidytract2016_sp_cent <- left_join(centroids_tt, tidytract2016_spatial,
                                   by = c('PID' = 'id'))

tidytract2016_sp_cent <- tidytract2016_sp_cent %>%
  dplyr::select(-PID, -hole, -STATEFP, -ALAND, -AWATER)

# depreciated
# write_csv(tidytract2016_sp_cent, '~/urban-forest/data/tidytract2016_sp_cent.csv')

tC <- read_csv('~/urban-forest/data/canopy_data.csv', col_names = T)

tscc <- left_join(tC, tidytract2016_sp_cent,
                  by = c("FIPS" = "GEOID"))
# 7 extra coverage values
tscc <- tscc %>%
  filter(!is.na(X)) %>%
  distinct(FIPS, .keep_all = T) %>%
  dplyr::select(-OBJECTID, -ZONE_CODE, -COUNT, -AREA, -MIN, -MAX, -RANGE, -MEAN, -STD, -SUM,
         -VARIETY, -MAJORITY, -MINORITY, -MEDIAN)


# 1-coverage for the real value
tscc <- tscc %>%
  mutate(sumOverCount = 1 - sumOverCount)


write_csv(tscc, '~/urban-forest/data/tscc.csv')
