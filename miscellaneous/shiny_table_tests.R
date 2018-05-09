newtidytract2016 <- left_join(tidytract2016, tractCoverage, by = c("fips" = "GEOID"))
newtidytract2016 <- newtidytract2016 %>%
  mutate("denominator" = 1 - percent_water,
         "% Canopy Coverage" = cover/denominator)
newtidytract2016 <- newtidytract2016[,c(1:56, 60)]
write_csv(newtidytract2016, "~/urban-forest/data/newtidytract2016.csv")

shinyApp(
  ui = fluidPage(
    selectInput("variable1", "Variable 1:",
                colnames(newtidytract2016[, c(3:5, 13:19, 27:57)]),
                selected = "% Population: Doctorate Degree"),
    selectInput("variable2", "Variable 2:",
                colnames(newtidytract2016[, c(3:5, 13:19, 27:57)]),
                selected = "% Canopy Coverage"),
    fluidRow(column(7, DT::dataTableOutput("mytable")),
             column(5, plotOutput("outplot")))
  ),
  server = function(input, output) {
    output$mytable <- DT::renderDataTable({
      newtidytract2016[, c("Census Tract", input$variable1, input$variable2), drop = FALSE]
    }, rownames = F)
    output$outplot <- renderPlot({
    s = input$mytable_rows_selected
    plot(newtidytract2016[, c(input$variable1, input$variable2), drop = FALSE])
    if (length(s)) points(newtidytract2016[s, c(input$variable1, input$variable2), drop = FALSE], pch = 19, cex = 2)
  })
})

