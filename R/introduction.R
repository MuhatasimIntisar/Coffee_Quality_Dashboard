# Overview tab
# ------------
# A bare snapshot of what the dataset contains — headline counts (coffees,
# countries, regions) and the nine sensory attributes. No commentary and no
# charts; placeholder copy stands in for any narrative to be added later.

introductionUI <- function(id) {
  ns <- NS(id)
  tagList(
    h2("Overview"),
    p(style = "max-width:760px; font-size:15px; color:#444;",
      "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod ",
      "tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim ",
      "veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea ",
      "commodo consequat."),

    # ── Headline counts ───────────────────────────────────────────────────────
    fluidRow(
      column(4, uiOutput(ns("kpi_coffees"))),
      column(4, uiOutput(ns("kpi_countries"))),
      column(4, uiOutput(ns("kpi_regions")))
    ),

    hr(),

    h4("Flavour profile attributes"),
    p(style = "max-width:760px; font-size:14px; color:#444;",
      "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod ",
      "tempor incididunt ut labore et dolore magna aliqua."),
    uiOutput(ns("attr_chips"))
  )
}

introductionServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {

    output$kpi_coffees <- renderUI(
      stat_card("Coffees", format(nrow(data), big.mark = ","), COFFEE_COLS$green))

    output$kpi_countries <- renderUI(
      stat_card("Countries",
                length(unique(data$Country.of.Origin[data$Country.of.Origin != ""])),
                COFFEE_COLS$blue))

    output$kpi_regions <- renderUI({
      pairs <- unique(paste(data$Country.of.Origin, data$Region)[data$Region != ""])
      stat_card("Regions", format(length(pairs), big.mark = ","), COFFEE_COLS$orange)
    })

    # The nine sensory attributes, shown as chips.
    output$attr_chips <- renderUI({
      div(style = "line-height:2.2;",
        lapply(FLAVOR_ATTRS, function(a)
          tags$span(style = paste0("display:inline-block; background:#f1e6ff;",
                                   "color:", COFFEE_COLS$purple, "; border-radius:6px;",
                                   "padding:3px 10px; font-size:13px; margin:0 6px 4px 0;"),
                    gsub("\\.", " ", a)))
      )
    })
  })
}
