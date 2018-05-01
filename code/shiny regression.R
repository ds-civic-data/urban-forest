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

m1 <- lm(population_density ~ white + black + med_family_income + dist,
         data = tidytract2016_sp_cent)
summary(m1)

m2 <- lm(population_density ~ white + black + med_family_income,
         data = tidytract2016_sp_cent)
summary(m2)

m3 <- lm(population_density ~ white + black + med_family_income + no_commute + below_hs + dist,
         data = tidytract2016_sp_cent)
summary(m3)
