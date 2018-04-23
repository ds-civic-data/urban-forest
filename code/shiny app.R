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
alpha_options <- c(1, 0.95, 0.9, 0.85, 0.8, 0.75, 0.7, 0.65, 0.6, 0.55, 0.5, 
                   0.45, 0.4, 0.35, 0.3, 0.25, 0.2, 0.15, 0.1, 0.05)
bounds_options <- c("black", "white", "transparent")
#min_tree <- nrow(tree_sample)
#max_tree <- nrow(tree_sample)


# Define UI for application that plots 
ui <- fluidPage(
  
  # Application title
  titlePanel("Adding a Reactive"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput("fill_opts", "Select Data Fill",
                  choices = select_fill_options),
      selectInput("bounds_opt", "Toggle Neighborhood Boundaries",
                  choices = bounds_options),
      # alpha opts could also be a slider
      selectInput("alpha_opts", "Tree Color Options",
                  choices = alpha_options),
      sliderInput("tree_sample", "Select Trees in Sample", 
                  min=0, max=216751, value=10000)
      # , dataTableOutput() -> data table for a selected neighborhood/censustract?
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("geom_map"))
  ))

##Server is where all of the computations happen
server <- function(input, output) {
  
  street_trees_all_filtered <- reactive({
    ##add code
    street_trees_all %>%
      sample_n(sample = input$tree_sample, replace = F)
  })
  
  output$geom_map <- renderPlot({
    
    portland %>%
      ggmap(base_layer = ggplot(lep_tidytract2016)) +
      geom_polygon(data = lep_tidytract2016, 
                   aes_string(x=long, y=lat, group=group.x, 
                              fill=input$fill_opts),
                   alpha = 0.6) +
      geom_polygon(data = neighborhoods_all, aes(x=long, y=lat, group=group), 
                   col = input$bounds_opt, fill = 'transparent') +
      geom_point(data = street_trees_all_filtered,
                 aes(x=X, y=Y), col = 'dark green',
                 alpha = input$alpha_opts, size = 0.75) +
      scale_fill_gradientn(colours = terrain.colors(7), na.value = 'transparent') +
      theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
      theme_bw()
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

