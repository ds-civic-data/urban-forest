shinyApp(
  ui = fluidPage(
    selectInput("variable1", "Variable 1:",
                c("Median Family Income" = "med_family_income",
                  "Unemployment Rate" = "unemployment_rate")),
    selectInput("variable2", "Variable 2:",
                c("Median Family Income" = "med_family_income",
                  "Unemployment Rate" = "unemployment_rate")),
   fluidRow(column(6, tableOutput("data")),
            column(6, plotOutput("outplot")))
  ),
  server = function(input, output) {
    output$data <- renderTable({
      tidytract2016[, c("qualifying_name", input$variable1, input$variable2), drop = FALSE]
    }, rownames = F)
    output$outplot <- renderPlot({
      ggplot(tidytract2016, aes_string(x = input$variable1)) +
        geom_point(aes_string(y = input$variable1))  })
    })
