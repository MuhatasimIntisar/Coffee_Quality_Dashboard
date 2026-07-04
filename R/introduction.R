# Overview tab — the welcome hub
# ------------------------------
# Greets the reader, then hands them five clickable highlight cards that jump
# straight into the other five tabs (details on demand). Below the hub: the
# evidence behind the dashboard (headline counts) and a friendly explainer of
# how coffee gets its score.

library(bslib)

# Plain-language meaning of each of the nine scored flavour attributes.
ATTR_DESC <- c(
  Aroma      = "How the coffee smells, from the dry grounds to the brewed cup.",
  Flavor     = "The overall taste character. The coffee's main impression on the palate.",
  Aftertaste = "The flavour that lingers once the coffee has been swallowed.",
  Acidity    = "Brightness or liveliness. A pleasant tang, not sourness.",
  Body       = "The weight and texture of the coffee in the mouth.",
  Balance    = "How well flavour, acidity, body and aftertaste work together.",
  Uniformity = "Consistency of flavour from cup to cup of the same coffee.",
  Clean.Cup  = "Freedom from off flavours or defects. A clean taste.",
  Sweetness  = "Natural sweetness, free from sour or harsh notes."
)

introductionUI <- function(id) {
  ns <- NS(id)
  tagList(
    # Hero: warm welcome beside a cup that slowly fills, steam drifting up.
    div(class = "hero-wrap",
        div(class = "hero-copy",
            h2("What makes a truly great cup of coffee?"),
            p(style = "max-width:720px; font-size:17px; color:#444; line-height:1.6;",
              "Somewhere between a hillside in Ethiopia and your kitchen, a coffee ",
              "earns its character. This dashboard follows that journey using ",
              "hundreds of independent quality gradings from professional tasters. ",
              "No jargon, no spreadsheets to squint at. Just the story of great ",
              "coffee, told with real data. Pick any card below and dive straight in.")),
        div(class = "cup-scene", `aria-hidden` = "true",
            span(class = "steam s1"), span(class = "steam s2"), span(class = "steam s3"),
            div(class = "cup",
                div(class = "cup-coffee", div(class = "cup-surface"))),
            div(class = "cup-handle"),
            div(class = "cup-saucer"))),

    # ── The five gateways: one clickable card per tab ─────────────────────────
    uiOutput(ns("hub")),

    hr(),

    # ── Why you can trust the findings ────────────────────────────────────────
    h4("Why you can trust what you see here"),
    p(style = "max-width:820px; font-size:14px; color:#444;",
      "None of this is opinion. Every insight is drawn from a large, independent ",
      "body of professional cup scores:"),
    layout_columns(
      col_widths = c(-1, 2, 2, 2, 2, 2, -1),
      uiOutput(ns("kpi_coffees")),
      uiOutput(ns("kpi_countries")),
      uiOutput(ns("kpi_regions")),
      uiOutput(ns("kpi_producers")),
      uiOutput(ns("kpi_partners"))
    ),

    hr(),

    # ── How coffee is rated ───────────────────────────────────────────────────
    h4("How a coffee earns its score"),
    p(style = "max-width:820px; font-size:14px; color:#444; line-height:1.6;",
      "Every coffee here has been tasted and scored by certified graders. Each ",
      "one gets a mark ", strong("out of 100"), ", built from ten parts scored ",
      "out of 10. Nine describe how the coffee tastes and feels (explained ",
      "below), and the tenth is the grader's own overall impression. ",
      textOutput(ns("scale_note"), inline = TRUE)),
    scorecard_card(plotOutput(ns("scorecard_ring"), height = "300px"),
                   uiOutput(ns("scorecard_legend"))),

    uiOutput(ns("glossary")),

    hr(),
    p(style = "font-size:17px; color:#2B2018; max-width:860px;",
      "Now you know how coffee is scored. A good place to begin is ",
      strong("where"), " the best of it grows: spin the globe on the ",
      strong("Global"), " tab.")
  )
}

introductionServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {

    nonblank <- function(x) x[!is.na(x) & x != ""]
    scored   <- data[!is.na(data$Total.Cup.Points) & data$Total.Cup.Points > 0, ]

    # ── Hub cards: each one is a clickable gateway into a tab ─────────────────
    # Clicking sets the top-level input `go_tab`, which server.R uses to switch.
    hub_card <- function(tab, kicker, big, desc, go, accent) {
      onclick <- sprintf(
        "Shiny.setInputValue('go_tab', '%s', {priority:'event'});", tab)
      tags$a(class = "hub-card", style = paste0("--hub-accent:", accent, ";"),
             href = "#", onclick = paste0(onclick, " return false;"),
             div(class = "hub-kicker", kicker),
             div(class = "hub-big",  big),
             div(class = "hub-desc", desc),
             span(class = "hub-go", go, span(class = "chev", HTML("&#8594;"))))
    }

    output$hub <- renderUI({
      n_countries <- length(unique(nonblank(data$Country.of.Origin)))
      by_country  <- summarise_by(scored, "Country.of.Origin")
      by_country  <- by_country[by_country$n_coffees >= 5, ]
      top_country <- by_country$group[which.max(by_country$avg_score)]

      div(class = "hub-grid",
        hub_card("Global", "Spin the globe",
                 sprintf("%d countries", n_countries),
                 "Every cup starts somewhere. Rotate a living 3D planet and see where the world grows its best beans.",
                 "Explore the world", CAT_COLS[1]),
        hub_card("Profile", "Taste an origin",
                 sprintf("%s leads", top_country),
                 "Meet the flavours behind each origin and pop open the charts, one delicious slice at a time.",
                 "See the flavours", CAT_COLS[4]),
        hub_card("Attributing Factors", "The why",
                 "Altitude matters",
                 "Mountains, moisture and method all leave fingerprints on flavour. See which ones matter most.",
                 "Find the drivers", CAT_COLS[2]),
        hub_card("Sensory Analysis", "Play barista",
                 "Build your cup",
                 "Tell us what you love with a few sliders and we will match you to your perfect coffee.",
                 "Make my coffee", CAT_COLS[3]),
        hub_card("Summary", "The big picture",
                 "6 takeaways",
                 "Short on time? The whole story, its winners and its lessons, on one beautiful page.",
                 "Read the story", CAT_COLS[6])
      )
    })

    # ── Headline evidence counts ──────────────────────────────────────────────
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
    sc_df <- scorecard_df(scored)
    output$scorecard_ring   <- renderPlot(scorecard_ring(sc_df))
    output$scorecard_legend <- renderUI(scorecard_legend(sc_df))
  })
}
