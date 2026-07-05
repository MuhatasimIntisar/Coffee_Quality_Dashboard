library(shiny)

# Each tab's logic lives in its module's server function (see R/).
# This file just starts each module, passing in the shared `coffee`
# dataset (loaded once in global.R). The id here must match ui.R.
#
# Cross-tab navigation:
#   * input$go_tab            — the Overview journey chips set this top-level
#                               input (via a small onclick); we switch to the
#                               named tab here.
#   * nav$country / nav$nonce — the Global tab asks to open Profile with a
#                               chosen origin preselected. Profile listens for
#                               the country; we switch tabs here.

server <- function(input, output, session) {
  nav <- reactiveValues(country = NULL, nonce = 0)

  introductionServer("introduction", coffee)
  locationServer("location", coffee, nav)
  profileServer("profile", coffee, nav)
  analysisServer("analysis", coffee)
  flavorServer("flavor", coffee)
  conclusionServer("conclusion", coffee)

  # An Overview journey chip was clicked -> jump straight to its tab.
  observeEvent(input$go_tab, {
    updateTabsetPanel(session, "tabs", selected = input$go_tab)
  })

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
        tags$li("Shafiya Rumana - 40507434")
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
      tags$hr(),
      tags$h5("References"),
      tags$ul(
        tags$li(tags$a(href = "https://www.w3schools.com/css/", target = "_blank",
                       "W3Schools — CSS Tutorial")),
        tags$li(tags$a(href = "https://www.w3schools.com/html/", target = "_blank",
                       "W3Schools — HTML Tutorial")),
        tags$li("R Shiny — ",
                tags$a(href = "https://shiny.posit.co/", target = "_blank",
                       "shiny.posit.co")),
        tags$li("fmsb R package — ",
                tags$a(href = "https://CRAN.R-project.org/package=fmsb", target = "_blank",
                       "CRAN.R-project.org/package=fmsb")),
        tags$li("DSA8045 Applied Analytics — course lectures and tutorial materials, ",
                "Queen's University Belfast.")
      ),
      tags$p(style = "font-size:12px; color:#6F5C49;",
             "Data: (Group5_coffee.csv). Built in R with ",
             "shiny, bslib, ggplot2, plotly, DT and maps.")
    ))
  })
}
