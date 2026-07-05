library(shiny)

# Each tab's logic lives in its module's server function (see R/).
# This file just starts each module, passing in the shared `coffee`
# dataset (loaded once in global.R). The id here must match ui.R.
#
# Cross-tab navigation uses one shared `nav` bus (a reactiveValues), so the
# whole app stays pure R with no hand-written JavaScript:
#   * nav$tab / nav$tab_nonce   — the Overview hub cards (actionLinks) ask to
#                                 open a named tab; we switch to it here.
#   * nav$country / nav$nonce   — the Global tab asks to open Profile with a
#                                 chosen origin preselected. Profile listens
#                                 for the country; we switch tabs here.

server <- function(input, output, session) {
  nav <- reactiveValues(country = NULL, nonce = 0, tab = NULL, tab_nonce = 0)

  introductionServer("introduction", coffee, nav)
  locationServer("location", coffee, nav)
  toneServer("tone", coffee, nav)
  analysisServer("analysis", coffee)
  flavorServer("flavor", coffee)
  conclusionServer("conclusion", coffee)

  # An Overview hub card was clicked -> jump straight to its tab.
  observeEvent(nav$tab_nonce, {
    req(nav$tab)
    updateTabsetPanel(session, "tabs", selected = nav$tab)
  }, ignoreInit = TRUE)

  # A country was clicked on the Global tab -> jump to its Profile.
  observeEvent(nav$nonce, {
    req(nav$country)
    updateTabsetPanel(session, "tabs", selected = "Profile")
  }, ignoreInit = TRUE)

  # ── About dialog: team credits + AI acknowledgement
  observeEvent(input$about_btn, {
    showModal(modalDialog(
      title = "About Dashboard",
      easyClose = TRUE,
      footer = modalButton("Close"),
      tags$p(tags$strong("Global Coffee Quality Assessment Platform"),
             " — DSA8045 Applied Analytics, Assignment 1."),
      tags$h5("Group Members"),
      tags$ul(
        tags$li("Muhatasim Intisar — 40497957"),
        tags$li("Siri Taranganahalli Gowda — 40503709 "),
        tags$li("Ishit Maheshbhai Patel — 40503430"),
        tags$li("Roshini . — 40497082"),
        tags$li("Shafiya Rumana - 40507434"),
      ),
      tags$hr(),
      tags$h5("AI Acknowledgement"),
      tags$p(
        "Generative AI (Claude) was used to assist in this assignment. The tool was",
        "used mainly for reviewing and debugging code and refining visualisations",
        "during the drafting phase. AI was also used to fix grammatical errors and refining ",
        "explanatory text. All analytical decisions, the interpretation of the ",
        "data, and the final content were written and ",
        "reviewed by the group members listed."),
      tags$p(style = "font-size:12px; color:#6F5C49;",
             "Data: (Group5_coffee.csv). Built in R with ",
             "shiny, bslib, ggplot2, plotly, DT and maps.")
    ))
  })
}
