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
tidytract2016 <- read_csv("~/urban-forest/data/tidytract2016.csv",
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
select_fill_options <- c("Total Population", "Population Density (Per Sq. Mile)", 
                         "Area (Land, in Sq. Miles)", "% Total Population: White Alone", 
                         "% Total Population: Black or African American Alone", 
                         "% Total Population: American Indian and Alaska Native Alone", 
                         "% Total Population: Asian Alone", "% Total Population: Native Hawaiian and Other Pacific Islander Alone", 
                         "% Total Population: Some Other Race Alone", "% Total Population: Two or More Races", 
                         "% Population: Less than High School", "% Population: High School Graduate", 
                         "% Population: Some College", "% Population: Bachelor's Degree", 
                         "% Population: Master's Degree", "% Population: Professional School", 
                         "% Population: Doctorate Degree", "% Civilian Population in Labor Force 16 Years and Over: Unemployed",
                         "Median Family Income", "Per Capita Income (In 2016 Inflation Adjusted Dollars)", 
                         "% Households: Less than $40,000", "% Households: $40,000 to $75,000",
                         "% Households: $75,000 to $125,000", "% Households: $125,000 and Over", 
                         "Gini Index", "% Households: No Earnings", "% Households: with Wage or Salary Income", 
                         "% Households: with Self-Employment Income", "% Households: with Interest, Dividends, or Net Rental Income", 
                         "% Households: with Social Security Income", "% Households: with Supplemental Security Income (Ssi)", 
                         "% Households: with Public Assistance Income", "% Households: with Retirement Income", 
                         "% Households: with Other Types of Income", "% Occupied Housing Units: Owner Occupied", 
                         "Median Gross Rent", "% Workers with Less than 20 Minute Commute",
                         "% Workers with 20 to 40 Minute Commute", "% Workers with Over 40 Minute Commute",
                         "% Workers with No Commute")
 
bounds_options <- c("black", "white", "transparent")

# Define UI for application that plots 
ui <- navbarPage(
  "Portland: Trees & Demographics", fluid = T, collapsable = T,
  tabPanel("Map Visualization",
           sidebarLayout(
             sidebarPanel(
               selectInput("fill_opts", "Select Data Fill",
                           choices = select_fill_options),
               checkboxInput("bounds_opts", "Toggle Neighborhood Boundaries",
                             value = FALSE),
               # alpha opts could also be a slider
               sliderInput("alpha_opts", "Select Tree Transparency",
                           min = 0, max = 1, value = 0.03),
               sliderInput("tree_sample", "Select Trees in Sample", 
                           min=0, max=216751, value=100000)),
             mainPanel(plotOutput("geom_map")))),
  tabPanel("Data Table Output",
           sidebarLayout(
             sidebarPanel(
               selectizeInput(
                 "show_vars",
                 "Columns to show:",
                 choices = colnames(tidytract2016),
                 multiple = TRUE,
                 selected = c("qualifying_name", "med_family_income", 
                              "unemployment_rate")
               ),
               actionButton("button", "Load Data"),
               uiOutput("category1")
             ),
             mainPanel(tableOutput("table")))),
  tabPanel("Regression Analysis"),
  tabPanel("README")
  
)


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
      geom_polygon(data = neighborhoods_all, aes(x=long, y=lat, group=group), 
                   col = input$bounds_opts, 
                   fill = 'transparent') +
      geom_point(data = street_trees_all_filtered(),
                 aes(x=X, y=Y #, size=`Canopy Coverage`
                 ), 
                 col = 'dark green',
                 size = 0.3,
                 alpha = input$alpha_opts)  +
      scale_fill_gradientn(colours = heat.colors(7), na.value = 'transparent') +
      theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
      theme_bw()
    
    
  })
  
  tidytract2016.react <- eventReactive(input$button, {
    tidytract2016[, input$show_vars]
  })
  observeEvent(input$button, {
    output$category1 <- renderUI({
      tidytract2016.sel <- tidytract2016.react()
      selectizeInput('cat1',
                     'Choose Tract',
                     choices = c("All", sort(as.character(
                       unique(tidytract2016.sel$qualifying_name)
                     ))),
                     selected = "All")
    })
    
    df_subset <- eventReactive(input$cat1, {
      tidytract2016.sel <- tidytract2016.react()
      if (input$cat1 == "All") {
        tidytract2016.sel
      }
      else{
        tidytract2016.sel[tidytract2016.sel$qualifying_name == input$cat1,]
      }
    })
    
    
    output$table <- renderTable({
      df_subset()
      
    })
  })
}

shinyApp(ui = ui, server = server)

