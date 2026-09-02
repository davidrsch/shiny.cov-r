library(shiny)

# Two different UI elements, a go button and a stop button, whose id
# string literals both live on the exact same physical source line below.
# Line-based coverage reduction (covr's own by-line min()) can only ever
# report one verdict for a shared physical line -- this fixture is what
# merge_ui_coverage()'s same-line collision message is meant to detect
# and warn about, and what ui_report()'s per-element table remains able to
# disambiguate even when the blended `cov` object cannot. (No quoted id
# text up here in the comments on purpose -- merge_ui_coverage()'s
# locate_by_text() does a literal source-text search and would happily
# match a comment first.)
ui <- fluidRow(actionButton("go", "Go"), actionButton("stop", "Stop"))

server <- function(input, output, session) {
}

shinyApp(ui = ui, server = server)
