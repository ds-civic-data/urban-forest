library(shiny)
library(tidyverse)
library(readr)
library(readxl)
library(lubridate)
library(DT)

tidytract2016 <- read_csv("tidytract2016.csv")

ui <- fluidPage(titlePanel("Test Explorer"),
                sidebarLayout(
                  sidebarPanel(
                    selectizeInput(
                      "show_vars",
                      "Columns to show:",
                      choices = colnames(tidytract2016),
                      multiple = TRUE,
                      selected = c("qualifying_name", "med_family_income", "unemployment_rate")
                    ),
                    actionButton("button", "Load Data"),
                    uiOutput("category1")
                  ),
                  mainPanel(tableOutput("table"))
                ))
server <- function(input, output, session) {
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

shinyApp(ui, server)