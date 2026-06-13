library(shiny)

# Each tab's logic lives in its module's server function (see R/).
# This file just starts each module, passing in the shared `coffee`
# dataset (loaded once in global.R). The id here must match ui.R.

server <- function(input, output, session) {
  introductionServer("introduction", coffee)
  geographyServer("geography", coffee)
  flavorServer("flavor", coffee)
  productionServer("production", coffee)
  conclusionServer("conclusion", coffee)
}
