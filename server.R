library(shiny)

# Each tab's logic lives in its module's server function (see R/).
# This file just starts each module, passing in the shared `coffee`
# dataset (loaded once in global.R). The id here must match ui.R.
#
# `nav` is a tiny shared bus for cross-tab navigation: the Global tab sets
# nav$country (and bumps nav$nonce) to ask the app to open the Profile tab with
# that country preselected. Profile listens for it; we switch tabs here.

server <- function(input, output, session) {
  nav <- reactiveValues(country = NULL, nonce = 0)

  introductionServer("introduction", coffee)
  locationServer("location", coffee, nav)
  profileServer("profile", coffee, nav)
  analysisServer("analysis", coffee)
  conclusionServer("conclusion", coffee)

  # A country was clicked on the Global tab -> jump to Profile.
  observeEvent(nav$nonce, {
    req(nav$country)
    updateTabsetPanel(session, "tabs", selected = "Profile")
  }, ignoreInit = TRUE)
}
