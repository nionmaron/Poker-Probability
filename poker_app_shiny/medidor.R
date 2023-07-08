library(shiny)
library(plotly)

ui <- fluidPage(
  plotlyOutput("gauge")
)

server <- function(input, output) {
  output$gauge <- renderPlotly({
    plot_ly(
      domain = list(x = c(0, 1), y = c(0, 1)),
      value = 5, # Você pode substituir este valor por uma variável reativa, se quiser.
      title = list(text = "Velocímetro"),
      type = "indicator",
      mode = "gauge+number",
      gauge = list(
        axis = list(range = list(0, 9)),
        bar = list(color = "darkblue"),
        steps = list(
          list(range = c(0, 1), color = "lightgreen"),
          list(range = c(1, 2), color = "greenyellow"),
          list(range = c(2, 3), color = "yellowgreen"),
          list(range = c(3, 4), color = "yellow"),
          list(range = c(4, 5), color = "gold"),
          list(range = c(5, 6), color = "orange"),
          list(range = c(6, 7), color = "orangered"),
          list(range = c(7, 8), color = "red"),
          list(range = c(8, 9), color = "darkred")
        )
      )
    )
  })
}

shinyApp(ui = ui, server = server)
