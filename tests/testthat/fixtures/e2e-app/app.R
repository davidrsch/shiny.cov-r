library(shiny)

# A real module, with a computed/looped input id inside it -- regression
# tests for source-line attribution for ids that have no literal string to
# text-search for (e.g. `ns(paste0("bin_", i))`) and for real
# module-boundary detection via the shiny::moduleServer() hook, instead of
# guessing from "-"-splitting an id after the fact.
counterModuleUI <- function(id) {
  ns <- NS(id)
  tagList(
    lapply(1:2, function(i) {
      sliderInput(ns(paste0("bin_", i)), paste("Bin", i), min = 0, max = 10, value = i)
    }),
    textOutput(ns("total"))
  )
}
counterModuleServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$total <- renderText({
      input$bin_1 + input$bin_2
    })
  })
}

ui <- fluidPage(
  # A hand-registered custom input binding, standing in for a third-party
  # widget library (e.g. shiny.fluent) with its own DOM/CSS conventions --
  # regression-tests that UI discovery works via Shiny's own binding
  # registry, not hardcoded CSS classes.
  tags$head(tags$script(HTML("
    var customBinding = new Shiny.InputBinding();
    $.extend(customBinding, {
      find: function(scope) { return $(scope).find('.totally-custom-widget'); },
      getValue: function(el) { return $(el).val(); },
      setValue: function(el, value) { $(el).val(value); },
      subscribe: function(el, callback) {
        $(el).on('change.customBinding', function(e) { callback(); });
      },
      unsubscribe: function(el) { $(el).off('.customBinding'); }
    });
    Shiny.inputBindings.register(customBinding, 'demo.customWidget');
  "))),
  tabsetPanel(
    tabPanel(
      "Main",
      selectInput("choice", "Choice", choices = c("a", "b"), selected = "a"),
      actionButton("greet", "Greet"), # discovered *and* tracked as interacted
      tags$input(id = "weird", class = "totally-custom-widget", type = "text"),
      fileInput("upload", "Upload a file"), # exercises upload_file()
      textOutput("result"),
      textOutput("uploadedName"),
      conditionalPanel(
        condition = "input.choice == 'b'",
        id = "onlyForB",
        textOutput("extra")
      )
    ),
    tabPanel("About", textOutput("about")),
    tabPanel("Counter", counterModuleUI("counter"))
  )
)

server <- function(input, output, session) {
  output$result <- renderText({
    if (input$choice == "a") {
      "Branch A"
    } else {
      "Branch B"
    }
  })
  output$extra <- renderText({
    "extra output"
  })
  output$uploadedName <- renderText({
    req(input$upload)
    input$upload$name
  })
  output$about <- renderText({
    "about text"
  })
  counterModuleServer("counter")
  # Exercises get_value(export = ...).
  exportTestValues(currentChoice = input$choice)
}

shinyApp(ui = ui, server = server)
