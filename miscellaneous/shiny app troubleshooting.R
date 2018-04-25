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
street_trees_all <- read_csv('~/urban-forest/data/street_trees_all.csv', 
                             col_names = T)
neighborhoods_all <- read_csv('~/urban-forest/data/neighborhoods_all.csv', 
                              col_names = T)
portland <- get_map(location = c(lon = -122.66, lat = 45.531), zoom = 11, 
                    maptype = "terrain")

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
                         "90_more_commute", "no_commute")

bounds_options <- c("black", "white", "transparent")


ui <- navbarPage(
  "Portland: Trees & Demographics", fluid = T, collapsable = T,
    tabPanel("Map Visualization",
               sidebarLayout(
                 sidebarPanel(
                   selectInput("fill_opts", "Select Data Fill",
                               choices = select_fill_options),
                   checkboxInput("bounds_opts", "Toggle Neighborhood Boundaries",
                               value = FALSE),
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
  tabPanel("Read Me Files")

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

# Run the application 
shinyApp(ui = ui, server = server)

