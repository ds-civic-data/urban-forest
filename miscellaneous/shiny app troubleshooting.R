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
installed.packages('leaflet')
library(leaflet)

set.seed(666)
##load the data
lep_tidytract2016 <- read_csv('~/urban-forest/data/lep_tidytract2016.csv', 
                              col_names = T)
tidytract2016_spatial <- read_csv('~/urban-forest/data/tidytract2016_spatial.csv',
                                  col_names = T)
tidytract2016_sp_cent <- read_csv('~/urban-forest/data/tidytract2016_sp_cent.csv',
                                  col_names = T)
tidytract2016 <- read_csv("~/urban-forest/data/tidytract2016.csv",
                          col_names = T)
street_trees_all <- read_csv('~/urban-forest/data/street_trees_all.csv', 
                             col_names = T)
neighborhoods_all <- read_csv('~/urban-forest/data/neighborhoods_all.csv', 
                              col_names = T)
#maps
portland <- get_map(location = c(lon = -122.66, lat = 45.531), zoom = 11, 
                    maptype = "terrain")
n_portland <- get_map(location = c(lon = -122.69, lat = 45.56), zoom = 12, 
                      maptype = "terrain")
se_portland <- get_map(location = c(lon = -122.59, lat = 45.48), zoom = 12, 
                       maptype = "terrain")
sw_portland <- get_map(location = c(lon = -122.73, lat = 45.485), zoom = 12, 
                       maptype = "terrain")
ne_portland <- get_map(location = c(lon = -122.59, lat = 45.55), zoom = 12, 
                       maptype = "terrain")
nw_portland <- get_map(location = c(lon = -122.745, lat = 45.54), zoom = 12, 
                       maptype = "terrain")

select_fill_options <- c("`Total Population`", "`Population Density (Per Sq. Mile)`", 
                         "`Area (Land, in Sq. Miles)`", "`% Total Population: White Alone`", 
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

bounds_options <- c("black", "white", "transparent")
quad_options <- c(n_portland, nw_portland, ne_portland, sw_portland,
                  se_portland)


ui <- navbarPage(
  "Portland: Trees & Demographics", fluid = T, collapsible = T,
    tabPanel("Map Visualization",
               sidebarLayout(
                 sidebarPanel(
                   selectInput("fill_opts", "Select Data Fill",
                               choices = colnames(tidytract2016_spatial[, c(18:71)])),
                   checkboxInput("bounds_opts", "Toggle Neighborhood Boundaries",
                               value = FALSE),
                   sliderInput("alpha_opts", "Select Tree Transparency",
                               min = 0, max = 1, value = 0.03),
                   sliderInput("tree_sample", "Select Trees in Sample", 
                               min=0, max=216751, value=100000),
                   selectInput("quad_opts", "Select City Quadrant",
                               choices = quad_options)),
                 mainPanel(
                   plotOutput("geom_map", click = "plot_click"),
                   h4('Clicked Tract'),
                   tableOutput('plot_clickedpoints'),
                   plotOutput("quadrant_map"))
                 #,
                 #sidebarPanel(
                #   selectInput("quad_opts", "Select City Quadrant",
                #               choices = quad_options),
                #   selectInput("fill_opts", "Select Data Fill",
                #               choices = select_fill_options),
                #   checkboxInput("bounds_opts", "Toggle Neighborhood Boundaries",
                #                 value = FALSE),
                #   sliderInput("alpha_opts", "Select Tree Transparency",
                #               min = 0, max = 1, value = 0.03),
                #   sliderInput("tree_sample", "Select Trees in Sample", 
                #               min=0, max=216751, value=100000)),
                # mainPanel(plotOutput('quadrant_map'))
                 )),
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
    tabPanel("Regression Analysis",
             sidebarLayout(
               sidebarPanel(
                 selectizeInput(
                   "show_vars_reg",
                   "Regression Variables:",
                   choices = colnames(tidytract2016_sp_cent[22:73]),
                   multiple = F,
                   selected = 'med_family_income'
                 ),
                 actionButton('button_reg', 'Load Regression'),
                 uiOutput("x_val")),
             mainPanel(uiOutput('regression'))
             ),
  tabPanel("About",
           mainPanel())

  ))


server <- function(input, output, session) {
  
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
  
  map_reactive <- reactive({
   # ifelse(coordinates(input$plot_click) %in% c(-122.67, -122.73, 45.53, 45.59),
    #  n_portland, 
     # ifelse(coordinates(input$plot_click) %in% c(-122.56, -122.63, 45.52, 45.58)),
    #  ne_portland, 
    #  ifelse(coordinates(input$plot_click) %in% c(-122.56, -122.63, 45.519999, 45.44)),
    #  se_portland, ifelse(coordinates(input$plot_click) %in% c(-122.69, -122.77, 45.52, 45.44)),
    #  sw_portland, nw_portland)
    
    input$quad_opts
  })
  
  output$quadrant_map <- renderPlot({
    
    map_reactive() %>%
      ggmap() +
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
                 aes(x=X, y=Y), 
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
  
  regression.react <- eventReactive(input$button_reg, {
    tidytract2016_sp_cent[, input$show_vars_reg]
  })
  
  
  observeEvent(input$button_reg, {
    reg <- renderUI({
      tidytract2016_sp_cent.lm <- tidytract2016_sp_cent.react()
      lm(population_density 
         # this ^ will be canopy coverage as soon as we have it
         ~ input$x_val + dist,
         data = tidytract2016_sp_cent)
      
      summary(tidytract2016_sp_cent.lm)
    })
    reg_subset <- eventReactive(input$x_val, {
      tidytract2016_sp_cent.lm <- tidytract2016_sp_cent.react()
      

      })
    })
  
  output$regression <- renderUI({
    reg()
    })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

