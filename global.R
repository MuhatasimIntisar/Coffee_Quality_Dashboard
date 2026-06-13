library(shiny)

# global.R runs ONCE when the app starts, before ui.R / server.R.
# Anything defined here is visible to every module, so we load the
# dataset here a single time and share it with all tabs.

coffee <- read.csv("Group5_coffee.csv", stringsAsFactors = FALSE)
