library(shiny)

# Source a helper file — this tests nested source() interception
source("helpers.R")

ui <- fluidPage(
  titlePanel("Multi-File Test App"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("bins", "Number of bins:",
                  min = 1, max = 50, value = 30)
    ),
    mainPanel(
      textOutput("label"),
      plotOutput("distPlot")
    )
  )
)

server <- function(input, output, session) {
  x <- faithful[, 2]

  output$label <- renderText({
    format_label(input$bins)
  })

  output$distPlot <- renderPlot({
    bins <- compute_bins(x, input$bins)
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
  })
}

shinyApp(ui = ui, server = server)
