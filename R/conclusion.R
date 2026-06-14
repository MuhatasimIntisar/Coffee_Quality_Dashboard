# Conclusion tab
# Owner: <name>
#
# Design: computed key-finding badges, two highlight stats (best country +
# best processing method), top 5 countries bar chart, and a takeaways
# section for the team to fill in.

# ── Highlight card helper ─────────────────────────────────────────────────────
highlight_card <- function(label, value) {
  div(
    style = paste("border-left:3px solid #1D9E75; padding:8px 14px;",
                  "margin-bottom:10px; border-radius:0 8px 8px 0;",
                  "background:#f7f7f7;"),
    tags$p(style = "font-size:12px; color:#888; margin:0 0 2px;", label),
    tags$p(style = "font-size:16px; font-weight:500; margin:0;", value)
  )
}

# ── UI ────────────────────────────────────────────────────────────────────────
conclusionUI <- function(id) {
  ns <- NS(id)
  tagList(
    h2("Conclusion"),
    p("A summary of the key findings from across the dashboard."),

    # Key finding badges
    uiOutput(ns("badges")),

    hr(),

    # Highlight stats + top countries chart
    fluidRow(
      column(4,
        h4("Highlights"),
        uiOutput(ns("highlight_country")),
        uiOutput(ns("highlight_method")),
        uiOutput(ns("highlight_altitude"))
      ),
      column(8,
        h4("Top 5 countries by average cup score"),
        plotOutput(ns("topCountriesPlot"), height = "260px")
      )
    ),

    hr(),

    # Takeaways — team fills these in
    h4("Key takeaways"),
    tags$ul(
      tags$li("Replace with your main finding about flavor drivers."),
      tags$li("Replace with your main finding about geography / altitude."),
      tags$li("Replace with your main finding about production / processing.")
    )
  )
}

# ── Server ────────────────────────────────────────────────────────────────────
conclusionServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {

    clean <- data[!is.na(data$Total.Cup.Points) & data$Total.Cup.Points > 0, ]

    # Average score per country (min 5 samples)
    country_avg <- reactive({
      avg <- tapply(clean$Total.Cup.Points, clean$Country.of.Origin, mean)
      n   <- table(clean$Country.of.Origin)
      sort(avg[n >= 5], decreasing = TRUE)
    })

    # Best processing method by avg score (min 5 samples)
    best_method <- reactive({
      m   <- trimws(clean$Processing.Method)
      avg <- tapply(clean$Total.Cup.Points, m, mean)
      n   <- table(m)
      avg <- sort(avg[n >= 5], decreasing = TRUE)
      names(avg)[1]
    })

    # ── Computed badges ───────────────────────────────────────────────────────
    output$badges <- renderUI({
      avg    <- country_avg()
      top_c  <- names(avg)[1]
      top_m  <- best_method()
      alt_ok <- !all(is.na(clean$altitude_mean_meters))

      badge_style <- function(bg, col) {
        sprintf("display:inline-block; background:%s; color:%s;
                 border-radius:6px; padding:4px 12px; font-size:13px;
                 margin:0 6px 8px 0;", bg, col)
      }

      div(
        tags$span(style = badge_style("#E1F5EE", "#0F6E56"),
                  paste("↑ Altitude correlates with quality")),
        tags$span(style = badge_style("#E6F1FB", "#185FA5"),
                  paste("\U0001F4CD", top_c, "leads in avg score")),
        tags$span(style = badge_style("#EEEDFE", "#534AB7"),
                  paste("\U0001F4A7", top_m, "= most consistent"))
      )
    })

    # ── Highlight stats ───────────────────────────────────────────────────────
    output$highlight_country <- renderUI({
      avg   <- country_avg()
      label <- paste0(names(avg)[1], " — ", round(avg[1], 1))
      highlight_card("Highest avg score", label)
    })

    output$highlight_method <- renderUI({
      highlight_card("Best processing method", best_method())
    })

    output$highlight_altitude <- renderUI({
      sub <- clean[!is.na(clean$altitude_mean_meters) &
                   clean$altitude_mean_meters > 0, ]
      if (nrow(sub) < 5) return(highlight_card("Altitude finding", "Insufficient data"))
      r <- round(cor(sub$altitude_mean_meters, sub$Total.Cup.Points,
                     use = "complete.obs"), 2)
      highlight_card("Altitude vs score (r)", as.character(r))
    })

    # ── Top 5 countries bar chart ─────────────────────────────────────────────
    output$topCountriesPlot <- renderPlot({
      avg <- country_avg()
      top <- rev(head(avg, 5))
      pal <- colorRampPalette(c("#9FE1CB", "#085041"))(5)
      par(mar = c(4, 10, 1, 3))
      bp <- barplot(top, horiz = TRUE, col = pal, border = NA,
                    las = 1, cex.names = 0.9,
                    xlim = c(min(top) - 2, max(top) + 1),
                    xlab = "Average Total Cup Points")
      text(x = top + 0.1, y = bp,
           labels = sprintf("%.1f", top),
           cex = 0.8, adj = 0, col = "#333333")
    })
  })
}
