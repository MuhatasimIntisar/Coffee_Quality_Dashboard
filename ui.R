library(shiny)

# Each tab is a Shiny module defined in its own file under R/ (auto-sourced).
# This file only lays out the overall page and slots each module's UI in.

ui <- fluidPage(
  titlePanel("Coffee Quality Dashboard"),

  navlistPanel(
    id = "tabs",
    widths = c(2, 10),
    well = TRUE,

    tabPanel("Introduction", introductionUI("introduction")),
    tabPanel("Geography",    geographyUI("geography")),
    tabPanel("Flavor",       flavorUI("flavor")),
    tabPanel("Production",   productionUI("production")),
    tabPanel("Conclusion",   conclusionUI("conclusion"))
  )
)
