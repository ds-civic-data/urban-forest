shinyApp(
  ui = fluidPage(
    selectInput("variable1", "Variable 1:",
                colnames(tidytract2016),
                selected = "med_family_income"),
    selectInput("variable2", "Variable 2:",
                colnames(tidytract2016),
                selected = "owner_occupied_units"),
    fluidRow(column(6, tableOutput("data")),
             column(6, plotOutput("outplot")))
  ),
  server = function(input, output) {
    output$data <- renderTable({
      tidytract2016[, c("qualifying_name", input$variable1, input$variable2), drop = FALSE]
    }, rownames = F)
    output$outplot <- renderPlot({
      ggplot(tidytract2016, aes_string(x = input$variable1, y = input$variable2)) +
        geom_point()  })
  })
