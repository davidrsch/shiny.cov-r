library(shiny)

# Uses a real third-party widget library (shinyWidgets), not a hand-rolled
# stand-in, to verify browser-registry discovery generalizes beyond base
# Shiny, and that source-line attribution holds against actual
# in-the-wild code -- not just reasoning about how such libraries are
# likely built.
#
# shinyWidgets::pickerInput() registers its own binding under
# "shinyWidgets.pickerInput" (not "shiny.*"), with its own CSS convention
# (".selectpicker", not ".shiny-input-container"). It also sets `id` via a
# *separate* htmltools::tagAppendAttributes() call after the bare tag is
# already built (`tag("select", ...)` first, id attached after) -- unlike
# every input in the other e2e fixture, which all set `id` directly in the
# tag() call. Building the picker ids in a loop via paste0() means there is
# also no literal id string in the source to text-search for, so this only
# passes at all if the tagAppendAttributes() hook (not just the tag() hook)
# correctly records id -> srcref.
ui <- fluidPage(
  lapply(1:2, function(i) {
    shinyWidgets::pickerInput(
      paste0("picker_", i),
      paste("Picker", i),
      choices = c("x", "y", "z")
    )
  }),
  textOutput("selected")
)

server <- function(input, output, session) {
  output$selected <- renderText({
    paste(input$picker_1, input$picker_2)
  })
}

shinyApp(ui = ui, server = server)
