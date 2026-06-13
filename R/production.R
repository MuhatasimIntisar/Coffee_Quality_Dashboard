# Production tab
# Owner: <name>
#
# A Shiny module = one UI function + one server function.
# Everything specific to this page lives in this file.

productionUI <- function(id) {
  ns <- NS(id)  # namespaces input/output ids so they can't clash with other tabs
  tagList(
    h2("Production")
    # Add your UI here, e.g. plotOutput(ns("myPlot")), tableOutput(ns("myTable"))
  )
}

productionServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    # `data` is the shared coffee dataset (loaded once in global.R).
    # Add your server logic here, e.g.
    # output$myPlot <- renderPlot({ hist(data$Total.Cup.Points) })
  })
}
