# Conclusion tab — findings and conclusions
# ------------------------------------------
# The three conditions that score highest (origin, altitude, moisture) as
# cards, then a formal written summary of what the analysis shows. The old
# coffee-finder table now lives on as the Sensory Analysis tab's coffee builder.

# A styled section heading: tiny mono kicker above a clean title.
sum_heading <- function(kicker, title) {
  div(style = "margin:6px 0 14px;",
      div(style = paste0("font-family:ui-monospace,Consolas,monospace; font-size:11px;",
                         "letter-spacing:.16em; text-transform:uppercase; color:#9C5A20;",
                         "margin-bottom:4px;"), kicker),
      h4(style = "margin:0;", title))
}

# One numbered finding card (styling in ui.R: .takeaway-card).
tk_card <- function(i, accent, title, body) {
  div(class = "takeaway-card", style = sprintf("--tk-accent:%s;", accent),
      div(class = "takeaway-num", sprintf("No. %d", i)),
      div(class = "takeaway-title", title),
      p(class = "takeaway-body", body))
}

conclusionUI <- function(id) {
  ns <- NS(id)
  tagList(
      h2("Conclusion"),
      p(style = "max-width:860px; font-size:17px; color:#444; line-height:1.6;",
        "This page consolidates the findings of the analysis. It first reports the ",
        "growing conditions associated with the highest average scores, then ",
        "summarises the sensory patterns that determine how coffees are graded."),

      sum_heading("Growing conditions", "What scores highest"),
      p(style = "color:#888; font-size:13px;",
        "The highest-scoring origin, growing altitude and bean moisture, ranked by ",
        "average cup score. Altitude and moisture bands require at least 20 graded ",
        "coffees to qualify."),
      fluidRow(
        column(4, uiOutput(ns("lift_country"))),
        column(4, uiOutput(ns("lift_altitude"))),
        column(4, uiOutput(ns("lift_moisture")))
      ),

      hr(),

      sum_heading("Key findings", "What the analysis shows"),
      div(class = "takeaway-grid",
        tk_card(1, CAT_COLS[1], "Aftertaste and flavour drive the score",
          paste0("Of the ten scored components, aftertaste and flavour track the ",
                 "overall grade most closely. A coffee's mark rests ",
                 "chiefly on its overall impression rather than on any single ",
                 "characteristic.")),
        tk_card(2, CAT_COLS[2], "The best coffees score consistently",
          paste0("As a leading note improves, the total score not only rises but ",
                 "tightens: high-scoring coffees closely resemble one another, while ",
                 "lower-scoring ones vary widely. Excellence is predictable; ",
                 "weakness is erratic.")),
        tk_card(3, CAT_COLS[3], "Altitude has a mild positive effect",
          paste0("Higher-grown coffees tend to score a little better, though the ",
                 "effect is modest — the altitude bands differ by only around ",
                 "two points. Cooler, thinner mountain air slows ripening and ",
                 "concentrates flavour.")),
        tk_card(4, CAT_COLS[4], "Low scores come from a single defect",
          paste0("Poor coffees rarely score uniformly low. Their totals collapse ",
                 "because one attribute fails — most often clean cup or ",
                 "sweetness — while the remaining notes stay unremarkable.")),
        tk_card(5, CAT_COLS[5], "Altitude and moisture act together",
          paste0("The two growing conditions matter most in combination: the ",
                 "highest averages come from higher-altitude farms whose beans ",
                 "retain a moderate moisture of roughly 10–12%, rather than from ",
                 "either factor alone.")),
        tk_card(6, CAT_COLS[6], "Processing method is a minor factor",
          paste0("Washed, natural and other processing methods show no meaningful ",
                 "relationship with the overall score. How a coffee is grown and ",
                 "graded matters considerably more than how it is processed."))
      ),

      hr(),

      sum_heading("In summary", "The overall picture"),
      p(style = "max-width:860px; font-size:16px; color:#2B2018; line-height:1.7;",
        "Taken together, the analysis supports a two-stage account of quality. ",
        "Growing conditions — altitude, moisture and origin — set the ceiling ",
        "for what a coffee can achieve, while the sensory attributes recorded at the ",
        "cupping table, led by aftertaste and flavour, determine the grade it ",
        "ultimately receives.")
  )
}

conclusionServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {

    scored <- data[!is.na(data$Total.Cup.Points) & data$Total.Cup.Points > 0, ]

    # Average score per country (>= 5 coffees for fairness).
    country_avg <- reactive({
      tab <- summarise_by(scored, "Country.of.Origin")
      tab <- tab[tab$n_coffees >= 5, ]
      tab[order(-tab$avg_score), ]
    })
    # ── What scores highest: origin, altitude band, moisture band ─────────────
    # Highest-scoring altitude band (same bands as the Attributing-Factors
    # heatmap; a band needs >= 20 graded coffees to qualify, for fairness).
    best_alt_band <- reactive({
      d <- scored[!is.na(scored$altitude_mean_meters) &
                  scored$altitude_mean_meters > 0 &
                  scored$altitude_mean_meters < 4000, ]
      d$band <- alt_band(d$altitude_mean_meters)
      agg  <- tapply(d$Total.Cup.Points, d$band, mean, na.rm = TRUE)
      keep <- names(table(d$band))[table(d$band) >= 20]
      agg  <- agg[keep]; agg <- agg[!is.na(agg)]
      if (!length(agg)) return(NULL)
      list(band = names(which.max(agg)), score = max(agg))
    })
    # Highest-scoring bean-moisture band (>= 20 graded coffees).
    best_moist_band <- reactive({
      d <- scored[!is.na(scored$Moisture) & scored$Moisture > 0, ]
      d$band <- moist_band(d$Moisture)
      agg  <- tapply(d$Total.Cup.Points, d$band, mean, na.rm = TRUE)
      keep <- names(table(d$band))[table(d$band) >= 20]
      agg  <- agg[keep]; agg <- agg[!is.na(agg)]
      if (!length(agg)) return(NULL)
      list(band = names(which.max(agg)), score = max(agg))
    })

    output$lift_country <- renderUI({
      ca <- country_avg()
      stat_card("Highest scoring country",
                paste0(ca$group[1], " at ", sprintf("%.1f", ca$avg_score[1])),
                COFFEE_COLS$green)
    })
    output$lift_altitude <- renderUI({
      b <- best_alt_band()
      v <- if (is.null(b)) "—" else paste0(b$band, " m at ", sprintf("%.1f", b$score))
      stat_card("Highest scoring altitude", v, COFFEE_COLS$blue)
    })
    output$lift_moisture <- renderUI({
      b <- best_moist_band()
      v <- if (is.null(b)) "—" else paste0(b$band, " at ", sprintf("%.1f", b$score))
      stat_card("Highest scoring moisture", v, COFFEE_COLS$orange)
    })
  })
}
