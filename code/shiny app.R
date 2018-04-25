## anything you load here can be seen by both ui and server

library(shiny)
library(tidyverse)
library(readr)
library(readxl)
library(lubridate)
library(sp)
library(rgdal)
library(ggmap)
library(maptools)
library(raster)

set.seed(666)
##load the data
lep_tidytract2016 <- read_csv('~/urban-forest/data/lep_tidytract2016.csv', 
                              col_names = T)
tidytract2016_spatial <- read_csv('~/urban-forest/data/tidytract2016_spatial.csv',
                                  col_names = T)
street_trees_thin <- read_csv('~/urban-forest/data/street_trees_thin.csv', 
                              col_names = T)
street_trees_all <- read_csv('~/urban-forest/data/street_trees_all.csv', 
                             col_names = T)
neighborhoods_all <- read_csv('~/urban-forest/data/neighborhoods_all.csv', 
                              col_names = T)
portland <- get_map(location = c(lon = -122.66, lat = 45.531), zoom = 11, 
                    maptype = "terrain")
##specify what categories we want to color with
select_fill_options <- c("total_population", "population_density", "white", 
                         "black", "am_indian", "asian", "pac_isl", "other_race",
                         "two_more_races", "below_hs", "hs_degree", "some_college",
                         "bachelors_degree", "masters_degree", 
                         "professional_school_degree", "doctorate_degree", 
                         "unemployment_rate", "hhs_less_10k", "hhs_10k_14999",
                         "hhs_15k_19999", "hhs_20k_24999", "hhs_25k_29999",
                         "hhs_30k_34999", "hhs_35k_39999", "hhs_40k_44999",
                         "hhs_45k_49999", "hhs_50k_59999", "hhs_60k_74999",
                         "hhs_75k_99999", "hhs_100k_124999", "hhs_125k_149999",
                         "hhs_150k_199999", "hhs_200k_more", "med_family_income",
                         "med_nonfamily_income", "hhs_with_no_earning",
                         "hhs_capital_income", "hhs_social_security", "hhs_ssi",
                         "hhs_public_assistance_income", "hhs_retirement_income",
                         "renter_occupied_units", "owner_occupied_units",
                         "10_less_commute", "10_19_commute", "20_29_commute",
                         "30_39_commute", "40_59_commute", "60_89_commute",
                         "90_more_commute", "no_commute", "Total_Pop_",
                         "Spanish", "Russian", "Chinese", "Japanese", "Korean",
                         "Mon_Khmer_", "Laotian", "Vietnamese", "Tagalog",
                         "Arabic", "African")
# need to rename some of these variables ^
bounds_options <- c("black", "white", "transparent")
#min_tree <- nrow(tree_sample)
#max_tree <- nrow(tree_sample)


# Define UI for application that plots 
ui <- fluidPage(
  
  # Application title
  titlePanel("Portland: Trees & Demographics"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput("fill_opts", "Select Data Fill",
                  choices = select_fill_options),
      selectInput("bounds_opt", "Toggle Neighborhood Boundaries",
                  choices = bounds_options),
      # alpha opts could also be a slider
      sliderInput("alpha_opts", "Tree Color Options",
                  min = 0, max = 1, value = 0.01),
      sliderInput("tree_sample", "Select Trees in Sample", 
                  min=0, max=216751, value=10000)
      # , dataTableOutput() -> data table for a selected neighborhood/censustract?
    ),
    
      mainPanel(
      plotOutput("geom_map"))
  ))


server <- function(input, output) {
  
  street_trees_all_filtered <- reactive({
    sample_n(street_trees_all, size = input$tree_sample, replace = F)
  })
  
  output$geom_map <- renderPlot({
    
    ggmap(portland) + 
      geom_polygon(data = tidytract2016_spatial, 
                   aes_string(x=tidytract2016_spatial$long, 
                              y=tidytract2016_spatial$lat, 
                              group=tidytract2016_spatial$group, 
                              fill=input$fill_opts), 
                   alpha = 0.6) +
      geom_polygon(data = neighborhoods_all, 
                   aes(x=long, y=lat, group=group), 
                   col = input$bounds_opt, fill = 'transparent') +
      geom_point(data = street_trees_all_filtered(),
                 aes(x=X, y=Y), col = 'dark green',
                 alpha = input$alpha_opts, size = 0.75) +
      scale_fill_gradientn(colours = heat.colors(7), na.value = 'transparent') +
      theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
      theme_bw()
  
     
  })
 
  #output$data_table <- renderTable({}) 
}


shinyApp(ui = ui, server = server)

