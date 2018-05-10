## packages

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
require(RColorBrewer)
library(leaflet)

set.seed(666)
##load the data
# this is our census dataset, combined with spatial variables for map plotting
tidytract2016_spatial <- read_csv('~/urban-forest/data/tidytract2016_spatial.csv',
                                  col_names = T)
# a smaller census dataset for franks table
tidytract2016 <- read_csv("~/urban-forest/data/tidytract2016.csv",
                          col_names = T)
# this is the full street trees dataset
street_trees_all <- read_csv('~/urban-forest/data/street_trees_all.csv', 
                             col_names = T)
# this is spatial lines for the neighborhood boundaries
neighborhoods_all <- read_csv('~/urban-forest/data/neighborhoods_all.csv', 
                              col_names = T)
newtidytract2016 <- read_csv('~/urban-forest/data/newtidytract2016.csv', 
                             col_names = T) %>%
  na.omit() %>%
  mutate(`% Canopy Coverage` = 1 - `% Canopy Coverage`) 

# this is a map of portland
portland <- get_map(location = c(lon = -122.66, lat = 45.531), zoom = 11, 
                    maptype = "terrain", color = "bw")

# for leaflet map
census_spatial <- readOGR(dsn=path.expand("~/urban-forest/data-raw/"), 
                          layer="cb_2017_41_tract_500k") 
z <- newtidytract2016 %>%
   filter(`Census Tract` != "Census Tract 9800") %>%
   mutate(`% Canopy Cover` = 1 - `% Canopy Coverage`) %>%
   na.omit()
#tract_stuff <- merge(census_spatial, z, by.x = "GEOID", by.y = "fips")
tract_stuff <- merge(census_spatial, newtidytract2016, by.x = "GEOID", by.y = "fips") 

##specify what categories we want to color with
Population <- list("`Total Population`", "`Population Density (Per Sq. Mile)`")
`Race & Ethnicity` <- list("`% Total Population: White Alone`", 
                       "`% Total Population: Black or African American Alone`", 
                       "`% Total Population: American Indian and Alaska Native Alone`", 
                       "`% Total Population: Asian Alone`", "`% Total Population: Native Hawaiian and Other Pacific Islander Alone`", 
                       "`% Total Population: Some Other Race Alone`", "`% Total Population: Two or More Races`")
`Education & Unemployment` <- list("`% Population: Less than High School`", "`% Population: High School Graduate`", 
               "`% Population: Some College`", "`% Population: Bachelor's Degree`", 
               "`% Population: Master's Degree`", "`% Population: Professional School`", 
               "`% Population: Doctorate Degree`", "`% Civilian Population in Labor Force 16 Years and Over: Unemployed`")
`Income & Equity` <- list("`Median Family Income`", "`Per Capita Income (In 2016 Inflation Adjusted Dollars)`", 
                       "`% Households: Less than $40,000`", "`% Households: $40,000 to $75,000`",
                       "`% Households: $75,000 to $125,000`", "`% Households: $125,000 and Over`", 
                       "`Gini Index`", "`% Households: No Earnings`", "`% Households: with Wage or Salary Income`", 
                       "`% Households: with Self-Employment Income`", "`% Households: with Interest, Dividends, or Net Rental Income`", 
                       "`% Households: with Social Security Income`", "`% Households: with Supplemental Security Income (Ssi)`", 
                       "`% Households: with Public Assistance Income`", "`% Households: with Retirement Income`", 
                       "`% Households: with Other Types of Income`")
Housing <- list("`% Occupied Housing Units: Owner Occupied`", 
             "`Median Gross Rent`")
`Commute Time` <- list("`% Workers with Less than 20 Minute Commute`",
                    "`% Workers with 20 to 40 Minute Commute`", "`% Workers with Over 40 Minute Commute`",
                    "`% Workers with No Commute`")
sfo <- list(Population, `Race & Ethnicity`, `Education & Unemployment`,
         `Income & Equity`, Housing, `Commute Time`)
select_fill_options <- c("`Total Population`", "`Population Density (Per Sq. Mile)`",
                       #  "`Area (Land, in Sq. Miles)`", 
                         "`% Total Population: White Alone`", 
                         "`% Total Population: Black or African American Alone`", 
                         "`% Total Population: American Indian and Alaska Native Alone`", 
                         "`% Total Population: Asian Alone`", "`% Total Population: Native Hawaiian and Other Pacific Islander Alone`", 
                         "`% Total Population: Some Other Race Alone`", "`% Total Population: Two or More Races`", 
                         "`% Population: Less than High School`", "`% Population: High School Graduate`", 
                         "`% Population: Some College`", "`% Population: Bachelor's Degree`", 
                         "`% Population: Master's Degree`", "`% Population: Professional School`", 
                         "`% Population: Doctorate Degree`", "`% Civilian Population in Labor Force 16 Years and Over: Unemployed`",
                         "`Median Family Income`", "`Per Capita Income (In 2016 Inflation Adjusted Dollars)`", 
                         "`% Households: Less than $40,000`", "`% Households: $40,000 to $75,000`",
                         "`% Households: $75,000 to $125,000`", "`% Households: $125,000 and Over`", 
                         "`Gini Index`", "`% Households: No Earnings`", "`% Households: with Wage or Salary Income`", 
                         "`% Households: with Self-Employment Income`", "`% Households: with Interest, Dividends, or Net Rental Income`", 
                         "`% Households: with Social Security Income`", "`% Households: with Supplemental Security Income (Ssi)`", 
                         "`% Households: with Public Assistance Income`", "`% Households: with Retirement Income`", 
                         "`% Households: with Other Types of Income`", "`% Occupied Housing Units: Owner Occupied`", 
                         "`Median Gross Rent`", "`% Workers with Less than 20 Minute Commute`",
                         "`% Workers with 20 to 40 Minute Commute`", "`% Workers with Over 40 Minute Commute`",
                         "`% Workers with No Commute`")
 
# for boundary toggle
bounds_options <- c("black", "white", "transparent")

# for leaflet map
leaflet_opts <- c("`Total Population`", "`Population Density (Per Sq. Mile)`",
                  #  "`Area (Land, in Sq. Miles)`", 
                  "`% Total Population: White Alone`", 
                  "`% Total Population: Black or African American Alone`", 
                  "`% Total Population: American Indian and Alaska Native Alone`", 
                  "`% Total Population: Asian Alone`", "`% Total Population: Native Hawaiian and Other Pacific Islander Alone`", 
                  "`% Total Population: Some Other Race Alone`", "`% Total Population: Two or More Races`", 
                  "`% Population: Less than High School`", "`% Population: High School Graduate`", 
                  "`% Population: Some College`", "`% Population: Bachelor's Degree`", 
                  "`% Population: Master's Degree`", "`% Population: Professional School`", 
                  "`% Population: Doctorate Degree`", "`% Civilian Population in Labor Force 16 Years and Over: Unemployed`",
                  "`Median Family Income`", "`Per Capita Income (In 2016 Inflation Adjusted Dollars)`", 
                  "`% Households: Less than $40,000`", "`% Households: $40,000 to $75,000`",
                  "`% Households: $75,000 to $125,000`", "`% Households: $125,000 and Over`", 
                  "`Gini Index`", "`% Households: No Earnings`", "`% Households: with Wage or Salary Income`", 
                  "`% Households: with Self-Employment Income`", "`% Households: with Interest, Dividends, or Net Rental Income`", 
                  "`% Households: with Social Security Income`", "`% Households: with Supplemental Security Income (Ssi)`", 
                  "`% Households: with Public Assistance Income`", "`% Households: with Retirement Income`", 
                  "`% Households: with Other Types of Income`", "`% Occupied Housing Units: Owner Occupied`", 
                  "`Median Gross Rent`", "`% Workers with Less than 20 Minute Commute`",
                  "`% Workers with 20 to 40 Minute Commute`", "`% Workers with Over 40 Minute Commute`",
                  "`% Workers with No Commute`", "`% Canopy Coverage`")

# Define UI for application that plots 
ui <- navbarPage(
  #title
  "Portland: Trees & Demographics", fluid = T, collapsible = T,
  # first panel: maps
  navbarMenu("Map Visualizations",
             tabPanel("Street Trees and Census Data",
                      sidebarLayout(
                        sidebarPanel(
                                      selectInput("fill_opts", "Select Data Fill",
                                                  choices = select_fill_options),
                                      checkboxInput("bounds_opts", "Toggle Neighborhood Boundaries",
                                                    value = FALSE),
                                      sliderInput("tree_sample", "Select Trees in Sample", 
                                                  min=0, max=100000, value=50000)),
                                    mainPanel(plotOutput("geom_map")))),
             tabPanel("Census Data Map Exploration",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("leaflet_opts", "Select Data Fill",
                                      choices = leaflet_opts)),
                        mainPanel(leafletOutput("leaflet_map"))))),
  # second panel: data table and base r scatterplot
  tabPanel("Data Table Output",
           selectInput("variable1", "Variable 1:",
                       list(colnames(newtidytract2016[,57]),
                            "Population & Land" = c(colnames(newtidytract2016[,3:5])),
                            "Race & Ethnicity" = c(colnames(newtidytract2016[,13:19])),
                            "Education" = c(colnames(newtidytract2016[,27:33])),
                            "Main Income & Employment Statistics" = c(colnames(newtidytract2016[,c(35,45,34,46)])),
                            "Household Income Sources" = c(colnames(newtidytract2016[,37:44])),
                            "Household Income Brackets" = c(colnames(newtidytract2016[,49:52])),
                            "Housing" = c(colnames(newtidytract2016[,47:48])),
                            "Commute Time" = c(colnames(newtidytract2016[,53:56]))),
                       selected = "% Canopy Coverage"),
           selectInput("variable2", "Variable 2:",
                       list(colnames(newtidytract2016[,57]),
                            "Population & Land" = c(colnames(newtidytract2016[,3:5])),
                            "Race & Ethnicity" = c(colnames(newtidytract2016[,13:19])),
                            "Education" = c(colnames(newtidytract2016[,27:33])),
                            "Main Income & Employment Statistics" = c(colnames(newtidytract2016[,c(35,45,34,46)])),
                            "Household Income Sources" = c(colnames(newtidytract2016[,37:44])),
                            "Household Income Brackets" = c(colnames(newtidytract2016[,49:52])),
                            "Housing" = c(colnames(newtidytract2016[,47:48])),
                            "Commute Time" = c(colnames(newtidytract2016[,53:56]))),
                       selected = "% Canopy Coverage"),
           fluidRow(column(7, DT::dataTableOutput("mytable")),
                    column(5, plotOutput("outplot"))),
           # output: interactive table
             mainPanel(tableOutput("table"))),
  # third panel: our spatial regression with diagnostics
  tabPanel("Regression Analysis",
           includeMarkdown('~/urban-forest/miscellaneous project information/Regression File.Rmd')),
  
  # fourth panel: our about page, including downloadable links to some data
  tabPanel("About",
           includeMarkdown('~/urban-forest/miscellaneous project information/About File.Rmd'))
  
)


server <- function(input, output) {
  
  # this reactive takes random subets of the trees data
  street_trees_all_filtered <- reactive({
    sample_n(street_trees_all, size = input$tree_sample, replace = F)
  })
  
  # this is the main map plot
  output$geom_map <- renderPlot({
    
    ggmap(portland) +
      geom_polygon(data = tidytract2016_spatial, 
                   aes_string(x=tidytract2016_spatial$long, 
                              y=tidytract2016_spatial$lat, 
                              group=tidytract2016_spatial$group, 
                              fill=input$fill_opts), 
                   alpha = 0.7) +
      geom_polygon(data = neighborhoods_all, aes(x=long, y=lat, group=group), 
                   col = input$bounds_opts, 
                   fill = 'transparent') +
      geom_point(data = street_trees_all_filtered(),
                 aes(x=X, y=Y #, size=`Canopy Coverage`
                 ), 
                 col = 'dark green',
                 size = 0.3,
                 alpha = 0.01)  +
      scale_fill_gradientn(colours = heat.colors(7), na.value = 'transparent') +
      theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
      theme_bw()
    
    
  })
  
  # This reactive expression represents the palette function,
  # which changes as the user makes selections in UI.
 # leaf_data <- reactive ({
 #   tract_stuff[tract_stuff@data == input$leaflet_opts]
 # })
  
  leaf_fill_data <- reactive({
    x <- input$leaflet_opts
    colorQuantile("OrRd", tract_stuff@data$x, n=9, na.color = "transparent")  
  })
  
  output$leaflet_map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    m <- leaflet(tract_stuff, options = leafletOptions(minZoom = 10, maxZoom = 13))
    m %>%
      addTiles() %>%
      setView(lng=-122.65, lat=45.52, zoom = 10) 
  })
  
  # Incremental changes to the map (in this case, replacing the
  # circles when a new color is chosen) should be performed in
  # an observer. Each independent set of things that can change
  # should be managed in its own observer.
  observe({
    
    x <- input$leaflet_opts
    
    leafletProxy("leaflet_map", data = tract_stuff) %>%
      clearShapes() %>%
      addPolygons(stroke = FALSE, fillOpacity = .8, smoothFactor = 0.2, 
                  color = ~leaf_fill_data())
      
  })
  
  # Use a separate observer to recreate the legend as needed.
  
  
  
  # output of table, dependent on drop down choices for variables 1 and 2
  output$mytable <- DT::renderDataTable({
    newtidytract2016[, c("Census Tract", input$variable1, input$variable2), drop = FALSE]
  }, rownames = F)
  # output of scatterplot, x = variable1, y = variable2, s records selected rows and adds points with 'if' line
  output$outplot <- renderPlot({
    s = input$mytable_rows_selected
    plot(newtidytract2016[, c(input$variable1, input$variable2), drop = FALSE])
    if (length(s)) points(newtidytract2016[s, c(input$variable1, input$variable2), drop = FALSE], pch = 19, cex = 2)
  })
      
}

shinyApp(ui = ui, server = server)

