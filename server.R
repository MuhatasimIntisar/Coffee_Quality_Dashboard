library(shiny)

# Each tab's logic lives in its module's server function (see R/).
# This file just starts each module, passing in the shared `coffee`
# dataset (loaded once in global.R). The id here must match ui.R.
#
# Cross-tab navigation:
#   * input$go_tab   — set by the Overview hub cards (plain JS onclick);
#                      whatever tab name arrives, we switch to it.
#   * `nav` bus      — the Global tab sets nav$country (and bumps nav$nonce)
#                      to open Profile with that origin preselected. Profile
#                      listens; we switch tabs here.

server <- function(input, output, session) {
  nav <- reactiveValues(country = NULL, nonce = 0)

  introductionServer("introduction", coffee)
  locationServer("location", coffee, nav)
  toneServer("tone", coffee, nav)
  analysisServer("analysis", coffee)
  flavorServer("flavor", coffee)
  conclusionServer("conclusion", coffee)

  # An Overview hub card was clicked -> jump straight to its tab.
  observeEvent(input$go_tab, {
    updateTabsetPanel(session, "tabs", selected = input$go_tab)
  })

  # A country was clicked on the Global tab -> jump to its Profile.
  observeEvent(nav$nonce, {
    req(nav$country)
    updateTabsetPanel(session, "tabs", selected = "Profile")
  }, ignoreInit = TRUE)

  # ── About dialog: team credits + AI acknowledgement ───────────────────────────
  # TODO: replace the placeholder names/IDs below with the real group members.
  observeEvent(input$about_btn, {
    showModal(modalDialog(
      title = "About this dashboard",
      easyClose = TRUE,
      footer = modalButton("Close"),
      tags$p(tags$strong("The World in Your Cup"),
             " — DSA8045 Applied Analytics, Assignment 1."),
      tags$h5("Created by"),
      tags$ul(
        tags$li("Student Name 1 — Student ID"),
        tags$li("Student Name 2 — Student ID"),
        tags$li("Student Name 3 — Student ID"),
        tags$li("Student Name 4 — Student ID"),
        tags$li("Student Name 5 — Student ID"),
        tags$li("Student Name 6 — Student ID")
      ),
      tags$hr(),
      tags$h5("Use of generative AI"),
      tags$p(
        "This dashboard was developed with assistance from a generative AI ",
        "assistant (Anthropic's Claude). It was used to help write and debug the ",
        "R and Shiny code, design and refine the visualisations, and draft ",
        "explanatory text. All analytical decisions, the interpretation of the ",
        "Coffee Quality Institute data, and the final content were directed and ",
        "reviewed by the group members listed above."),
      tags$p(style = "font-size:12px; color:#6F5C49;",
             "Data: Coffee Quality Institute (Group5_coffee.csv). Built in R with ",
             "shiny, bslib, ggplot2, plotly, DT and maps.")
    ))
  })
}
