newtidytract2016 <- left_join(tidytract2016, canopy_data, by = c("fips" = "FIPS")) %>%
  rename("% Canopy Coverage" = "sumOverCount")
newtidytract2016 <- newtidytract2016[,c(1:56, 72)]
write_csv(newtidytract2016, "~/urban-forest/data/newtidytract2016.csv")

shinyApp(
  ui = fluidPage(
    selectInput("variable1", "Variable 1:",
                list(colnames(newtidytract2016[,57]),
                     "Population & Land" = c(colnames(newtidytract2016[,3:5])),
                     "Race & Ethnicity" = c(colnames(newtidytract2016[,13:19])),
                     "Education" = c(colnames(newtidytract2016[,27:33])),
                     "Main Income & Employment Statistics" = c(colnames(newtidytract2016[,c(33,45,34,46)])),
                     "Household Income Sources" = c(colnames(newtidytract2016[,37:44])),
                     "Housing" = c(colnames(newtidytract2016[,49:52])),
                     "Commute Time" = c(colnames(newtidytract2016[,53:56]))),
                selected = "% Canopy Coverage"),
    selectInput("variable2", "Variable 2:",
                list(colnames(newtidytract2016[,57]),
                     "Population & Land" = c(colnames(newtidytract2016[,3:5])),
                     "Race & Ethnicity" = c(colnames(newtidytract2016[,13:19])),
                     "Education" = c(colnames(newtidytract2016[,27:33])),
                     "Main Income & Employment Statistics" = c(colnames(newtidytract2016[,c(33,45,34,46)])),
                     "Household Income Sources" = c(colnames(newtidytract2016[,37:44])),
                     "Housing" = c(colnames(newtidytract2016[,49:52])),
                     "Commute Time" = c(colnames(newtidytract2016[,53:56]))),
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

