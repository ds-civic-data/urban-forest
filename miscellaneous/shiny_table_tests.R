shinyApp(
  ui = fluidPage(
    selectInput("variable1", "Variable 1:",
                colnames(tidytract2016[, c(3:5, 13:19, 27:56)]),
                selected = "% Population: Doctorate Degree"),
    selectInput("variable2", "Variable 2:",
                colnames(tidytract2016[, c(3:5, 13:19, 27:56)]),
                selected = "Median Family Income"),
    fluidRow(column(7, DT::dataTableOutput("mytable")),
             column(5, plotOutput("outplot")))
  ),
  server = function(input, output) {
    output$mytable <- DT::renderDataTable({
      tidytract2016[, c("Census Tract", input$variable1, input$variable2), drop = FALSE]
    }, rownames = F)
    output$outplot <- renderPlot({
    s = input$mytable_rows_selected
    plot(tidytract2016[, c(input$variable1, input$variable2), drop = FALSE])
    if (length(s)) points(tidytract2016[s, c(input$variable1, input$variable2), drop = FALSE], pch = 19, cex = 2)
  })
})

