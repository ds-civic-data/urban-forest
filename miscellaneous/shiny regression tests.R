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

tractCoverage <- read_csv('~/urban-forest/data/tractCoverage.csv', col_names = T)

tscc <- left_join(tractCoverage, tidytract2016_sp_cent,
                            by = c("GEOID" = "GEOID"))
# 7 extra coverage values
tt_sp_cent_cov <- tscc %>%
  filter(!is.na(cent_x)) 

m4 <- lm(cover ~ below_hs, tt_sp_cent_cov)
summary(m4)



write_csv(tt_sp_cent_cov, '~/urban-forest/data/tidytract2016_sp_cent.csv')
