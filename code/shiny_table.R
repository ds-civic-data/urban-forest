library(shiny)
library(tidyverse)
library(readr)
library(readxl)
library(lubridate)
library(DT)

tidytract2016 <- read_csv("data/tidytract2016.csv")

column_options <- c("total_population", "population_density", "area", "white", "black", "am_indian", "asian", 
                    "pac_isl", "other_race", "two_more_races", "below_hs", "hs_degree", "some_college", 
                    "bachelors_degree", "masters_degree", "professional_school_degree", "doctorate_degree", 
                    "unemployment_rate", "hhs_less_10k", "hhs_10k_14999", "hhs_15k_19999", "hhs_20k_24999", 
                    "hhs_25k_29999", "hhs_30k_34999", "hhs_35k_39999", "hhs_40k_44999", "hhs_45k_49999", 
                    "hhs_50k_59999", "hhs_60k_74999", "hhs_75k_99999", "hhs_100k_124999", "hhs_125k_149999",
                    "hhs_150k_199999", "hhs_200k_more", "med_family_income", "med_nonfamily_income", 
                    "hhs_with_no_earnings", "hhs_capital_income", "hhs_social_security", "hhs_ssi", 
                    "hhs_public_assistance_income", "hhs_retirement_income", "renter_occupied_units", 
                    "owner_occupied_units", "10_less_commute", "10_19_commute", "20_29_commute", "30_39_commute", 
                    "40_59_commute", "60_89_commute", "90_more_commute", "no_commute")

shinyApp(
  ui = fluidPage(DTOutput('tabletract')),
  server = function(input, output) {
    output$tabletract = renderDT(
      tidytract2016, options = list(lengthChange = FALSE)
    )
  }
)
