# Overview tab
# ------------
# Sets up the whole dashboard's question, then earns the reader's trust, then
# explains the scoring. Flow, top to bottom:
#   1. What this dashboard is for (the explainer framing)
#   2. The evidence behind it — headline counts (why you can trust the findings)
#   3. How coffee is rated — the 0–100 scale and a plain-language glossary of the
#      nine flavour attributes.

library(bslib)

# Plain-language meaning of each of the nine scored flavour attributes.
ATTR_DESC <- c(
  Aroma      = "How the coffee smells — from the dry grounds to the brewed cup.",
  Flavor     = "The overall taste character; the coffee's main impression on the palate.",
  Aftertaste = "The flavour that lingers once the coffee has been swallowed.",
  Acidity    = "Brightness or liveliness — a pleasant tang, not sourness.",
  Body       = "The weight and texture of the coffee in the mouth.",
  Balance    = "How well flavour, acidity, body and aftertaste work together.",
  Uniformity = "Consistency of flavour from cup to cup of the same coffee.",
  Clean.Cup  = "Freedom from off-flavours or defects — a clean taste.",
  Sweetness  = "Natural sweetness, free from sour or harsh notes."
)

introductionUI <- function(id) {
  ns <- NS(id)
  tagList(
    h2("What makes a great coffee — and where does it come from?"),
    p(style = "max-width:820px; font-size:17px; color:#444; line-height:1.6;",
      "This dashboard explores what separates an exceptional coffee from an ",
      "ordinary one — and where in the world the best beans are grown — using ",
      "independent quality gradings from the Coffee Quality Institute. Move ",
      "through the tabs to see ", strong("where"), " great coffee comes from ",
      "(Global), ", strong("what"), " growing and processing factors drive ",
      "quality (Analysis), a close-up of any single ", strong("origin"),
      " (Profile), and the headline ", strong("conclusions"), " (Summary)."),

    hr(),

    # ── Why you can trust the findings — the scale of the evidence ─────────────
    h4("Why you can trust these findings"),
    p(style = "max-width:820px; font-size:14px; color:#444;",
      "These aren't opinions — every insight is drawn from a large, independent ",
      "body of professional cup-scores:"),
    layout_columns(
      col_widths = c(-1, 2, 2, 2, 2, 2, -1),
      uiOutput(ns("kpi_coffees")),
      uiOutput(ns("kpi_countries")),
      uiOutput(ns("kpi_regions")),
      uiOutput(ns("kpi_producers")),
      uiOutput(ns("kpi_partners"))
    ),

    hr(),

    # ── How coffee is rated — the scale + the attribute glossary ───────────────
    h4("How coffee is rated"),
    p(style = "max-width:820px; font-size:14px; color:#444; line-height:1.6;",
      "Every coffee here has been cup-scored by certified tasters. They rate it ",
      strong("out of 100"), " — built from ten components each scored out of 10: ",
      "nine describe distinct flavour characteristics (below), and the tenth is ",
      "the grader's own overall mark. ",
      textOutput(ns("scale_note"), inline = TRUE)),
    scorecard_card(plotOutput(ns("scorecard_ring"), height = "300px"),
                   uiOutput(ns("scorecard_legend"))),

    uiOutput(ns("glossary")),

    hr(),
    p(style = "font-size:17px; color:#2B2018; max-width:860px;",
      "Now you know how coffee is scored — start with ", strong("where"),
      " the best comes from on the ", strong("Global"), " tab.")
  )
}

introductionServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {

    nonblank <- function(x) x[!is.na(x) & x != ""]

    kpi_h <- "120px"   # fixed height so all five cards stay the same size
    output$kpi_coffees <- renderUI(
      stat_card("Coffees evaluated", format(nrow(data), big.mark = ","),
                COFFEE_COLS$green, height = kpi_h))
    output$kpi_countries <- renderUI(
      stat_card("Countries", length(unique(nonblank(data$Country.of.Origin))),
                COFFEE_COLS$blue, height = kpi_h))
    output$kpi_regions <- renderUI({
      pairs <- unique(paste(data$Country.of.Origin, data$Region)[data$Region != ""])
      stat_card("Growing regions", format(length(pairs), big.mark = ","),
                COFFEE_COLS$orange, height = kpi_h)
    })
    output$kpi_producers <- renderUI(
      stat_card("Producers", format(length(unique(nonblank(data$Producer))),
                                    big.mark = ","), COFFEE_COLS$purple, height = kpi_h)
    )
    output$kpi_partners <- renderUI(
      stat_card("Grading partners",
                length(unique(nonblank(data$In.Country.Partner))),
                COFFEE_COLS$green, height = kpi_h))

    # The nine attributes, each with a plain-language description. Each chip is
    # coloured to match its slice in the ring (same cat_cols palette / order).
    output$glossary <- renderUI({
      pal <- setNames(cat_cols(length(FLAVOR_ATTRS) + 1),
                      c(FLAVOR_ATTRS, "Cupper.Points"))
      # Dark or white text depending on how light the chip colour is.
      text_on <- function(bg) {
        r <- col2rgb(bg)
        lum <- (0.299 * r[1] + 0.587 * r[2] + 0.114 * r[3]) / 255
        if (lum > 0.6) "#2B2018" else "#FFFFFF"
      }
      chip <- function(name, bg) tags$span(
        style = paste0("display:inline-block; background:", bg, "; color:",
                       text_on(bg), "; border-radius:6px; padding:3px 10px;",
                       "font-size:13px; font-weight:600; min-width:96px;",
                       "text-align:center;"), name)
      div(style = "max-width:820px;",
        lapply(names(ATTR_DESC), function(a)
          div(style = "display:flex; align-items:baseline; gap:12px; margin-bottom:9px;",
              chip(gsub("\\.", " ", a), pal[[a]]),
              tags$span(style = "color:#444; font-size:14px; line-height:1.5;",
                        ATTR_DESC[[a]])))
      )
    })

    # Where scores actually land in THIS dataset (no external threshold).
    output$scale_note <- renderText({
      d <- data$Total.Cup.Points[!is.na(data$Total.Cup.Points) & data$Total.Cup.Points > 0]
      sprintf("In this dataset, coffees range from %.0f to %.0f points, averaging %.0f.",
              min(d), max(d), mean(d))
    })

    # Average scorecard: a ring + a progress-bar legend (shared helpers).
    sc_df <- scorecard_df(data[!is.na(data$Total.Cup.Points) & data$Total.Cup.Points > 0, ])
    output$scorecard_ring   <- renderPlot(scorecard_ring(sc_df))
    output$scorecard_legend <- renderUI(scorecard_legend(sc_df))
  })
}
