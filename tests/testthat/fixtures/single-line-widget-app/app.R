library(shiny)

# Deliberately the idiomatic single-line case: the whole
# UI-construction statement for the go button below is a complete,
# standalone statement on its own line, with nothing else on that line.
# This makes the synthetic entry's srcref byte-for-byte identical to the
# real covr entry's srcref for that same statement, unless
# merge_ui_coverage() guards against it. (No quoted id text up here in the
# comments on purpose -- merge_ui_coverage()'s locate_by_text() does a
# literal source-text search and would happily match a comment first.)
ui <- actionButton("go", "Go")

server <- function(input, output, session) {
}

shinyApp(ui = ui, server = server)
