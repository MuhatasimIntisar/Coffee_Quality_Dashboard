# Production tab
# Owner: <name>
#
# Design: 3 stat cards (total bags, avg altitude, no. of processing methods),
# processing method breakdown (horizontal bar), bags per harvest year (line),
# and altitude vs Total Cup Points scatter plot.

# ── Stat card helper ──────────────────────────────────────────────────────────
prod_stat_card <- function(label, value) {
  div(
    style = paste("background:#f7f7f7; border-radius:8px; padding:12px 16px;",
                  "margin-bottom:10px; border:0.5px solid #e0e0e0;"),
    tags$p(style = "font-size:12px; color:#888; margin:0 0 4px;", label),
    tags$p(style = "font-size:22px; font-weight:500; margin:0;", value)
  )
}

# ── UI ────────────────────────────────────────────────────────────────────────
productionUI <- function(id) {
  ns <- NS(id)
  tagList(
    h2("Production"),
    p("How coffee is grown, processed, and distributed — altitude, processing method, harvest year, and volume."),

    # Stat cards
    fluidRow(
      column(4, uiOutput(ns("card_bags"))),
      column(4, uiOutput(ns("card_altitude"))),
      column(4, uiOutput(ns("card_methods")))
    ),

    hr(),

    # Charts row 1
    fluidRow(
      column(6,
        h4("Processing method breakdown"),
        plotOutput(ns("methodPlot"), height = "260px")
      ),
      column(6,
        h4("Bags per harvest year"),
        plotOutput(ns("yearPlot"), height = "260px")
      )
    ),

    br(),

    # Altitude scatter
    h4("Altitude vs. Total Cup Points"),
    p("Does growing altitude affect cup quality?"),
    plotOutput(ns("altitudePlot"), height = "300px")
  )
}

# ── Server ────────────────────────────────────────────────────────────────────
productionServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {

    clean <- data[!is.na(data$Total.Cup.Points) & data$Total.Cup.Points > 0 &
                  !is.na(data$Number.of.Bags), ]

    # ── Stat cards ────────────────────────────────────────────────────────────
    output$card_bags <- renderUI({
      total <- sum(clean$Number.of.Bags, na.rm = TRUE)
      prod_stat_card("Total bags", formatC(total, format = "d", big.mark = ","))
    })

    output$card_altitude <- renderUI({
      avg <- mean(clean$altitude_mean_meters, na.rm = TRUE)
      val <- if (is.nan(avg)) "—" else paste0(round(avg), " m")
      prod_stat_card("Avg altitude", val)
    })

    output$card_methods <- renderUI({
      n <- length(unique(trimws(clean$Processing.Method[clean$Processing.Method != ""])))
      prod_stat_card("Processing methods", as.character(n))
    })

    # ── Processing method breakdown ───────────────────────────────────────────
    output$methodPlot <- renderPlot({
      counts <- sort(table(trimws(clean$Processing.Method)), decreasing = FALSE)
      counts <- counts[names(counts) != ""]
      pal <- colorRampPalette(c("#B5D4F4", "#185FA5"))(length(counts))
      par(mar = c(4, 12, 1, 3))
      bp <- barplot(counts, horiz = TRUE, col = pal, border = NA,
                    las = 1, cex.names = 0.8, xlab = "Number of coffees")
      text(x = counts + max(counts) * 0.01, y = bp,
           labels = as.character(counts), cex = 0.75, adj = 0, col = "#333333")
    })

    # ── Bags per harvest year ─────────────────────────────────────────────────
    output$yearPlot <- renderPlot({
      yrs <- suppressWarnings(as.numeric(sub(".*?(\\d{4}).*", "\\1", clean$Harvest.Year)))
      sub <- clean[!is.na(yrs) & yrs >= 2009 & yrs <= 2018, ]
      sub$year_num <- suppressWarnings(as.numeric(sub(".*?(\\d{4}).*", "\\1", sub$Harvest.Year)))
      agg <- aggregate(Number.of.Bags ~ year_num, data = sub, FUN = sum)
      agg <- agg[order(agg$year_num), ]

      if (nrow(agg) == 0) {
        plot.new(); text(0.5, 0.5, "No year data available."); return()
      }

      par(mar = c(4, 5, 1, 2))
      plot(agg$year_num, agg$Number.of.Bags,
           type = "o", pch = 21, bg = "#378ADD", col = "#185FA5",
           lwd = 2, cex = 1.3,
           xlab = "Harvest year", ylab = "Total bags",
           xaxt = "n")
      axis(1, at = agg$year_num, labels = agg$year_num, cex.axis = 0.85)
    })

    # ── Altitude vs score scatter ─────────────────────────────────────────────
    output$altitudePlot <- renderPlot({
      sub <- clean[!is.na(clean$altitude_mean_meters) &
                   clean$altitude_mean_meters > 0 &
                   clean$altitude_mean_meters < 4000, ]

      if (nrow(sub) == 0) {
        plot.new(); text(0.5, 0.5, "No altitude data available."); return()
      }

      par(mar = c(5, 5, 1, 2))
      plot(sub$altitude_mean_meters, sub$Total.Cup.Points,
           pch = 21, col = adjustcolor("#534AB7", 0.5),
           bg  = adjustcolor("#7F77DD", 0.3), cex = 0.9,
           xlab = "Altitude (metres above sea level)",
           ylab = "Total Cup Points")

      if (nrow(sub) > 2) {
        fit <- lm(Total.Cup.Points ~ altitude_mean_meters, data = sub)
        abline(fit, col = "#534AB7", lwd = 2, lty = 2)
        r <- cor(sub$altitude_mean_meters, sub$Total.Cup.Points, use = "complete.obs")
        legend("topleft", bty = "n",
               legend = paste("r =", round(r, 2)), text.col = "#534AB7")
      }
    })
  })
}
