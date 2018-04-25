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


fluidPage(
  
  title = 'Select Table Rows',
  
  h1('ACS Data'),
  
  fluidRow(
    column(6, DT::dataTableOutput('x1')),
    column(6, plotOutput('x2', height = 500))
  ),
  
  hr(),
  
  h1('A Server-side Table'),
  
  fluidRow(
    column(9, DT::dataTableOutput('x3')),
    column(3, verbatimTextOutput('x4'))
  )
  
)
shinyServer(function(input, output, session) {
  
  output$x1 = DT::renderDataTable(tidytract2016, server = FALSE)
  
  # highlight selected rows in the scatterplot
  output$x2 = renderPlot({
    s = input$x1_rows_selected
    par(mar = c(4, 4, 1, .1))
    plot(tidytract2016)
    if (length(s)) points(tidytract2016[s, , drop = FALSE], pch = 19, cex = 2)
  })
  
  # server-side processing
  tidytract20162 = tidytract2016[, 1:8]
  output$x3 = DT::renderDataTable(tidytract20162, server = TRUE)
  
  # print the selected indices
  output$x4 = renderPrint({
    s = input$x3_rows_selected
    if (length(s)) {
      cat('These rows were selected:\n\n')
      cat(s, sep = ', ')
    }
  })
  
})