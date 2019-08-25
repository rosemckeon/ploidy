disturploidy_landscape <- function(dataset) {

  library(shiny)
  library(tidyverse)
  library(ggplot2)
  vars = names(dataset)

  shinyApp(
    ui = fluidPage(
      fluidRow(
        style = "padding-bottom: 20px;",
        column(12, sliderInput('gen', 'Generation', min = 0, max = 25, step = 1, width = "100%"))
      ),
      fluidRow(
        plotOutput('landscape', height = "400px")
      )
    ),

    server = function(input, output, session) {

      # Filter the data reactively to the correct generation
      selectedData = reactive({
        dataset %>% filter(gen == input$gen)
      })

      output$landscape = renderPlot(
        height = 400, {
          qplot(
            X, Y,
            data = selectedData,
            geom = "count"
          ) + theme_classic() + theme(
            legend.position = "top"
          )
        }
      )
    },

    options = list(height = 500)
  )
}