library(shiny)

ui <- fluidPage(
  titlePanel("Simple Test App"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("bins", "Number of bins:",
                  min = 1, max = 50, value = 30),
      actionButton("go", "Update")
    ),
    mainPanel(
      plotOutput("distPlot")
    )
  )
)

server <- function(input, output, session) {
  x <- faithful[, 2]

  output$distPlot <- renderPlot({
    input$go
    bins <- seq(min(x), max(x), length.out = isolate(input$bins) + 1)
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
  })
}

shinyApp(ui = ui, server = server)
