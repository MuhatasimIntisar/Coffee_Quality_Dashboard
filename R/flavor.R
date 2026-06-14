# Flavor tab
# Owner: <name>
#
# Design: cascading filters (Processing Method → Country → Region),
# radar chart of 9 sensory attributes, horizontal bar chart of avg scores,
# and 4 stat cards (avg score, top/lowest attribute, sample count).

FLAVOR_ATTRS <- c("Aroma", "Flavor", "Aftertaste", "Acidity",
                  "Body", "Balance", "Uniformity", "Clean.Cup", "Sweetness")

# ── Radar chart helper (base R) ───────────────────────────────────────────────
draw_radar <- function(scores, title = "") {
  n      <- length(scores)
  angles <- seq(0, 2 * pi, length.out = n + 1)[-(n + 1)]
  min_val <- 6; max_val <- 10
  norm    <- pmax(0, pmin(1, (scores - min_val) / (max_val - min_val)))

  px <- norm * cos(angles - pi / 2)
  py <- norm * sin(angles - pi / 2)
  lx <- 1.32 * cos(angles - pi / 2)
  ly <- 1.32 * sin(angles - pi / 2)

  par(mar = c(1, 1, 2, 1))
  plot(0, 0, type = "n", xlim = c(-1.6, 1.6), ylim = c(-1.6, 1.6),
       asp = 1, axes = FALSE, xlab = "", ylab = "", main = title, cex.main = 0.95)

  # Grid rings
  for (r in c(0.25, 0.5, 0.75, 1.0)) {
    gx <- r * cos(seq(0, 2 * pi, length.out = 200) - pi / 2)
    gy <- r * sin(seq(0, 2 * pi, length.out = 200) - pi / 2)
    lines(gx, gy, col = "#DDDDDD", lwd = 0.8)
    text(0, r + 0.03, sprintf("%.1f", min_val + r * (max_val - min_val)),
         cex = 0.5, col = "#AAAAAA")
  }

  # Spokes
  for (i in seq_len(n))
    lines(c(0, cos(angles[i] - pi/2)), c(0, sin(angles[i] - pi/2)),
          col = "#DDDDDD", lwd = 0.8)

  # Filled polygon
  polygon(c(px, px[1]), c(py, py[1]),
          col = adjustcolor("#1D9E75", alpha.f = 0.2),
          border = "#1D9E75", lwd = 2)
  points(px, py, pch = 21, bg = "#1D9E75", col = "white", cex = 1.6, lwd = 1.5)

  # Labels
  for (i in seq_len(n)) {
    lab <- gsub("\\.", " ", names(scores)[i])
    cx  <- cos(angles[i] - pi / 2)
    adj_x <- if (cx < -0.1) 1 else if (cx > 0.1) 0 else 0.5
    text(lx[i], ly[i], lab, cex = 0.72, col = "#444444", adj = c(adj_x, 0.5))
  }
}

# ── Stat card helper ──────────────────────────────────────────────────────────
stat_card <- function(label, value) {
  div(
    style = paste("background:#f7f7f7; border-radius:8px; padding:12px 16px;",
                  "margin-bottom:10px; border:0.5px solid #e0e0e0;"),
    tags$p(style = "font-size:12px; color:#888; margin:0 0 4px;", label),
    tags$p(style = "font-size:20px; font-weight:500; margin:0;", value)
  )
}

# ── UI ────────────────────────────────────────────────────────────────────────
flavorUI <- function(id) {
  ns <- NS(id)
  tagList(
    h2("Flavor"),
    p("Explore the sensory profile of coffees. Use the filters to focus on a specific origin or processing method."),

    wellPanel(
      fluidRow(
        column(3,
          selectInput(ns("process"), "Processing Method",
                      choices = "All Methods", selected = "All Methods")
        ),
        column(3,
          selectInput(ns("country"), "Country",
                      choices = "All Countries", selected = "All Countries")
        ),
        column(3,
          selectInput(ns("region"), "Region",
                      choices = "All Regions", selected = "All Regions")
        ),
        column(3,
          br(),
          uiOutput(ns("sample_info"))
        )
      )
    ),

    fluidRow(
      column(6,
        h4("Flavor profile"),
        plotOutput(ns("radar"), height = "360px")
      ),
      column(6,
        h4("Average score per attribute"),
        plotOutput(ns("bar"), height = "360px")
      )
    ),

    hr(),

    fluidRow(
      column(3, uiOutput(ns("card_avg"))),
      column(3, uiOutput(ns("card_top"))),
      column(3, uiOutput(ns("card_low"))),
      column(3, uiOutput(ns("card_n")))
    )
  )
}

# ── Server ────────────────────────────────────────────────────────────────────
flavorServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {

    clean <- data[!is.na(data$Total.Cup.Points) & data$Total.Cup.Points > 0, ]

    # Populate processing method filter on startup
    observe({
      methods <- sort(unique(trimws(clean$Processing.Method)))
      methods <- methods[methods != ""]
      updateSelectInput(session, "process",
                        choices = c("All Methods", methods),
                        selected = "All Methods")
    })

    # Country choices update when processing method changes
    observeEvent(input$process, {
      sub <- if (input$process == "All Methods") clean
             else clean[trimws(clean$Processing.Method) == input$process, ]
      countries <- sort(unique(trimws(sub$Country.of.Origin)))
      countries <- countries[countries != ""]
      updateSelectInput(session, "country",
                        choices = c("All Countries", countries),
                        selected = "All Countries")
    })

    # Region choices update when country changes
    observeEvent(list(input$process, input$country), {
      sub <- clean
      if (!is.null(input$process) && input$process != "All Methods")
        sub <- sub[trimws(sub$Processing.Method) == input$process, ]
      if (!is.null(input$country) && input$country != "All Countries")
        sub <- sub[trimws(sub$Country.of.Origin) == input$country, ]
      regions <- sort(unique(trimws(sub$Region)))
      regions <- regions[regions != ""]
      updateSelectInput(session, "region",
                        choices = c("All Regions", regions),
                        selected = "All Regions")
    })

    # Filtered data
    filtered <- reactive({
      sub <- clean
      if (!is.null(input$process) && input$process != "All Methods")
        sub <- sub[trimws(sub$Processing.Method) == input$process, ]
      if (!is.null(input$country) && input$country != "All Countries")
        sub <- sub[trimws(sub$Country.of.Origin) == input$country, ]
      if (!is.null(input$region) && input$region != "All Regions")
        sub <- sub[trimws(sub$Region) == input$region, ]
      sub
    })

    # Average scores for the 9 attributes
    avg_scores <- reactive({
      sub <- filtered()
      if (nrow(sub) == 0)
        return(setNames(rep(NA_real_, length(FLAVOR_ATTRS)), FLAVOR_ATTRS))
      sapply(FLAVOR_ATTRS, function(a) mean(sub[[a]], na.rm = TRUE))
    })

    # Dynamic chart title
    chart_title <- reactive({
      parts <- c()
      if (!is.null(input$process) && input$process != "All Methods")  parts <- c(parts, input$process)
      if (!is.null(input$country) && input$country != "All Countries") parts <- c(parts, input$country)
      if (!is.null(input$region)  && input$region  != "All Regions")  parts <- c(parts, input$region)
      if (length(parts) == 0) "All coffees" else paste(parts, collapse = " · ")
    })

    no_data_plot <- function() {
      plot.new()
      text(0.5, 0.5, "No data for this selection.", cex = 1.1, col = "#888888")
    }

    # Radar chart
    output$radar <- renderPlot({
      scores <- avg_scores()
      if (any(is.na(scores))) { no_data_plot(); return() }
      draw_radar(scores, title = chart_title())
    })

    # Horizontal bar chart
    output$bar <- renderPlot({
      scores <- avg_scores()
      if (any(is.na(scores))) { no_data_plot(); return() }
      ord    <- order(scores)
      labels <- gsub("\\.", " ", names(scores))
      pal    <- colorRampPalette(c("#9FE1CB", "#085041"))(length(scores))
      par(mar = c(4, 8, 1, 3))
      bp <- barplot(scores[ord], names.arg = labels[ord],
                    horiz = TRUE, col = pal, border = NA,
                    xlim = c(6, 10.3), las = 1,
                    cex.names = 0.85, xlab = "Average score")
      text(x = scores[ord] + 0.05, y = bp,
           labels = sprintf("%.2f", scores[ord]),
           cex = 0.78, adj = 0, col = "#333333")
    })

    # Sample size
    output$sample_info <- renderUI({
      n <- nrow(filtered())
      tags$p(style = "color:#888; font-size:13px; margin-top:8px;",
             sprintf("%d coffee sample%s", n, if (n == 1) "" else "s"))
    })

    # Stat cards
    output$card_avg <- renderUI({
      scores <- avg_scores()
      val <- if (any(is.na(scores))) "—" else sprintf("%.1f", mean(scores))
      stat_card("Avg sensory score", val)
    })

    output$card_top <- renderUI({
      scores <- avg_scores()
      if (any(is.na(scores))) return(stat_card("Top attribute", "—"))
      stat_card("Top attribute", gsub("\\.", " ", names(which.max(scores))))
    })

    output$card_low <- renderUI({
      scores <- avg_scores()
      if (any(is.na(scores))) return(stat_card("Lowest attribute", "—"))
      stat_card("Lowest attribute", gsub("\\.", " ", names(which.min(scores))))
    })

    output$card_n <- renderUI({
      stat_card("Coffees in selection", as.character(nrow(filtered())))
    })
  })
}
