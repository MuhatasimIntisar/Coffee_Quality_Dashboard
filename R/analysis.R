# Analysis tab  — bslib sidebar + stacked card sections
# ------------------------------------------------------
# Global filters (country / method / years) are pinned in the left sidebar.
# The body is four sections that flow downwards — Timeline, Correlation,
# Heatmap, Ranking — each a row with the chart card on the LEFT and its own
# selection card on the RIGHT. A full-width Coffee finder closes the tab (it
# needs no right-hand selection).

library(DT)
library(bslib)

# Scatter x-axis options (continuous factors) and heatmap layouts.
XVAR_CHOICES <- c("Altitude (m)" = "altitude_mean_meters",
                  "Moisture (%)" = "Moisture")
HEAT_CHOICES <- c("Altitude × Moisture" = "alt_moist",
                  "Altitude × Method"   = "alt_method")
# What the Ranking section can rank.
ENTITY_COLS <- c("Country"           = "Country.of.Origin",
                 "Region"            = "Region",
                 "Producer"          = "Producer",
                 "Processing method" = "Processing.Method")

analysisUI <- function(id) {
  ns <- NS(id)
  tagList(
    h2("Analysis: trends, factors, ranking & finder"),
    p("Explore how where and how coffee is grown relates to quality and flavour. ",
      "The filters on the left scope every section; each section below has its ",
      "own controls on the right. Use the finder at the bottom to pull out the ",
      "specific coffees that match a category."),

    layout_sidebar(
      fillable = FALSE,

      # ── Global filters ──────────────────────────────────────────────────────
      sidebar = sidebar(
        title = "Filters",
        width = 300,
        selectInput(ns("country"), "Country",           choices = "All countries"),
        selectInput(ns("method"),  "Processing method", choices = "All methods"),
        sliderInput(ns("years"), "Harvest year range",
                    min = YEAR_RANGE[1], max = YEAR_RANGE[2],
                    value = YEAR_RANGE, step = 1, sep = "", width = "100%")
      ),

      # ── 1. Timeline ─────────────────────────────────────────────────────────
      layout_columns(
        col_widths = c(8, 4),
        card(full_screen = TRUE,
             card_header(textOutput(ns("timeline_title"))),
             plotOutput(ns("timeline"), height = "320px")),
        card(card_header("Timeline options"),
             card_body(
               selectInput(ns("measure_tl"), "Measure (y)",
                           choices = MEASURE_CHOICES, selected = "Total.Cup.Points"),
               p(class = "card-note",
                 "Mean of the chosen measure across harvest years.")))
      ),

      # ── 2. Correlation ──────────────────────────────────────────────────────
      layout_columns(
        col_widths = c(8, 4),
        card(full_screen = TRUE,
             card_header(textOutput(ns("scatter_title"))),
             plotOutput(ns("scatter"), height = "340px")),
        card(card_header("Correlation options"),
             card_body(
               selectInput(ns("xvar_corr"), "X variable",
                           choices = XVAR_CHOICES, selected = "altitude_mean_meters"),
               selectInput(ns("measure_corr"), "Y measure",
                           choices = MEASURE_CHOICES, selected = "Total.Cup.Points"),
               p(class = "card-note",
                 "Each point is a coffee, coloured by method; the dashed line is a linear fit.")))
      ),

      # ── 3. Heatmap ──────────────────────────────────────────────────────────
      layout_columns(
        col_widths = c(8, 4),
        card(full_screen = TRUE,
             card_header(textOutput(ns("heat_title"))),
             plotOutput(ns("heat"), height = "360px")),
        card(card_header("Heatmap options"),
             card_body(
               selectInput(ns("measure_heat"), "Measure (fill)",
                           choices = MEASURE_CHOICES, selected = "Total.Cup.Points"),
               selectInput(ns("heat"), "Layout",
                           choices = HEAT_CHOICES, selected = "alt_moist"),
               p(class = "card-note",
                 "Mean of the measure in each cell; blank cells have no coffees.")))
      ),

      # ── 4. Ranking ──────────────────────────────────────────────────────────
      layout_columns(
        col_widths = c(8, 4),
        card(full_screen = TRUE,
             card_header(textOutput(ns("rank_title"))),
             plotOutput(ns("ranking"), height = "380px")),
        card(card_header("Ranking options"),
             card_body(
               selectInput(ns("rank_entity"), "Rank",
                           choices = ENTITY_COLS, selected = "Country.of.Origin"),
               selectInput(ns("rank_metric"), "By metric",
                           choices = METRIC_CHOICES, selected = "avg_score"),
               sliderInput(ns("rank_min"), "Min coffees per group",
                           min = 1, max = 30, value = 5, step = 1),
               p(class = "card-note",
                 "Top 15 groups; the minimum keeps tiny samples off the chart.")))
      ),

      # ── Coffee finder (full width, no right-hand card) ──────────────────────
      card(
        card_header("Coffee finder"),
        card_body(
          p(class = "card-note",
            "Every coffee in the current slice. Use the column filters to narrow ",
            "to a specific category — country, region, method, or an altitude / ",
            "moisture / score range."),
          DTOutput(ns("finder")))
      )
    )
  )
}

analysisServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {

    countries <- sort(unique(data$Country.of.Origin[data$Country.of.Origin != ""]))
    methods   <- sort(unique(data$Processing.Method[data$Processing.Method != ""]))
    updateSelectInput(session, "country", choices = c("All countries", countries))
    updateSelectInput(session, "method",  choices = c("All methods", methods))

    # Scored coffees passing the global country / method / year filters.
    base <- reactive({
      d <- data[!is.na(data$Total.Cup.Points) & data$Total.Cup.Points > 0, ]
      if (!is.null(input$country) && input$country != "All countries")
        d <- d[d$Country.of.Origin == input$country, ]
      if (!is.null(input$method) && input$method != "All methods")
        d <- d[d$Processing.Method == input$method, ]
      d[!is.na(d$harvest_year) &
        d$harvest_year >= input$years[1] & d$harvest_year <= input$years[2], ]
    })

    # Altitude band helper (shared by the heatmap).
    alt_band <- function(x) {
      cut(x, c(0, 1000, 1250, 1500, 1750, 2000, Inf),
          c("<1000", "1000–1250", "1250–1500", "1500–1750", "1750–2000", "2000+"),
          right = FALSE)
    }

    # ── 1. Timeline: mean measure per harvest year ──────────────────────────────
    output$timeline_title <- renderText(
      sprintf("%s over time", measure_label(input$measure_tl)))
    output$timeline <- renderPlot({
      m <- input$measure_tl
      d <- base(); d <- d[!is.na(d[[m]]), ]
      if (nrow(d) == 0) return(gg_no_data())
      ag <- aggregate(d[[m]], list(year = d$harvest_year), mean, na.rm = TRUE)
      names(ag) <- c("year", "val")
      ggplot(ag, aes(year, val)) +
        geom_line(colour = COFFEE_COLS$blue, linewidth = 1) +
        geom_point(colour = COFFEE_COLS$blue, size = 2.6) +
        scale_x_continuous(breaks = ag$year) +
        labs(x = "Harvest year", y = paste("Mean", measure_label(m))) + theme_coffee()
    })

    # ── 2. Correlation: x variable vs measure, coloured by method ───────────────
    output$scatter_title <- renderText({
      xl <- names(XVAR_CHOICES)[XVAR_CHOICES == input$xvar_corr]
      sprintf("%s vs %s", measure_label(input$measure_corr), xl)
    })
    output$scatter <- renderPlot({
      m <- input$measure_corr; xv <- input$xvar_corr
      d <- base(); d <- d[!is.na(d[[m]]) & !is.na(d[[xv]]), ]
      if (xv == "altitude_mean_meters")
        d <- d[d[[xv]] > 0 & d[[xv]] < 4000, ]
      else
        d <- d[d[[xv]] > 0, ]
      if (nrow(d) < 2) return(gg_no_data("Not enough data for this slice."))
      d$xplot  <- if (xv == "Moisture") d[[xv]] * 100 else d[[xv]]
      d$method <- ifelse(d$Processing.Method == "", "Unknown", d$Processing.Method)
      xl  <- names(XVAR_CHOICES)[XVAR_CHOICES == xv]
      pal <- colorRampPalette(CAT_COLS)(length(unique(d$method)))
      ggplot(d, aes(xplot, .data[[m]])) +
        geom_point(aes(colour = method), alpha = 0.55, size = 2) +
        geom_smooth(method = "lm", se = TRUE, colour = COFFEE_COLS$purple,
                    fill = COFFEE_COLS$purple, alpha = 0.12, linetype = "dashed") +
        scale_colour_manual(values = pal) +
        labs(x = xl, y = measure_label(m), colour = "Method") + theme_coffee()
    })

    # ── 3. Heatmap: altitude band × (moisture band | method), filled by measure ─
    output$heat_title <- renderText(
      if (input$heat == "alt_method")
        sprintf("%s by altitude × method", measure_label(input$measure_heat))
      else
        sprintf("%s by altitude × moisture", measure_label(input$measure_heat)))
    output$heat <- renderPlot({
      m <- input$measure_heat
      d <- base(); d <- d[!is.na(d[[m]]), ]
      d <- d[!is.na(d$altitude_mean_meters) & d$altitude_mean_meters > 0 &
             d$altitude_mean_meters < 4000, ]
      if (nrow(d) == 0) return(gg_no_data())
      d$altband <- alt_band(d$altitude_mean_meters)

      if (input$heat == "alt_method") {
        d <- d[d$Processing.Method != "", ]
        if (nrow(d) == 0) return(gg_no_data())
        ag <- aggregate(d[[m]], list(x = d$altband, y = d$Processing.Method),
                        mean, na.rm = TRUE)
        ylab <- "Processing method"
      } else {
        d <- d[!is.na(d$Moisture) & d$Moisture > 0, ]
        if (nrow(d) == 0) return(gg_no_data())
        d$moband <- cut(d$Moisture, c(0, 0.10, 0.11, 0.12, 0.13, Inf),
                        c("<10%", "10–11%", "11–12%", "12–13%", "13%+"), right = FALSE)
        ag <- aggregate(d[[m]], list(x = d$altband, y = d$moband), mean, na.rm = TRUE)
        ylab <- "Moisture"
      }
      names(ag) <- c("x", "y", "val")
      ggplot(ag, aes(x, y, fill = val)) +
        geom_tile(colour = "white") +
        geom_text(aes(label = sprintf("%.1f", val)), size = 3, colour = "#333333") +
        scale_fill_gradient(low = "#F1E4CE", high = COFFEE_COLS$blue) +
        labs(x = "Altitude band (m)", y = ylab, fill = measure_label(m)) +
        theme_coffee() + theme(axis.text.x = element_text(angle = 25, hjust = 1))
    })

    # ── 4. Ranking: top groups by a chosen metric ───────────────────────────────
    output$rank_title <- renderText({
      el <- names(ENTITY_COLS)[ENTITY_COLS == input$rank_entity]
      sprintf("%s ranked by %s", el, tolower(METRIC_LABELS[[input$rank_metric]]))
    })
    output$ranking <- renderPlot({
      req(input$rank_entity, input$rank_metric %in% names(METRIC_LABELS))
      a <- summarise_by(base(), input$rank_entity)
      a <- a[a$n_coffees >= input$rank_min & !is.na(a[[input$rank_metric]]), ]
      if (nrow(a) == 0) return(gg_no_data("No groups meet the minimum sample size."))
      a <- head(a[order(-a[[input$rank_metric]]), ], 15)
      a$group <- factor(a$group, levels = rev(a$group))
      ggplot(a, aes(.data[[input$rank_metric]], group)) +
        geom_col(fill = COFFEE_COLS$green, width = 0.72) +
        scale_x_continuous(expand = expansion(mult = c(0, 0.08))) +
        labs(x = METRIC_LABELS[[input$rank_metric]], y = NULL) + theme_coffee()
    })

    # ── Coffee finder (per-column filters) ──────────────────────────────────────
    output$finder <- renderDT({
      d <- base()
      tab <- data.frame(
        Country  = d$Country.of.Origin, Region = d$Region, Producer = d$Producer,
        Year     = d$harvest_year, Method = d$Processing.Method,
        Altitude = round(d$altitude_mean_meters),
        Moisture = round(d$Moisture * 100, 1),
        Score    = round(d$Total.Cup.Points, 2),
        Aroma    = d$Aroma, Flavor = d$Flavor, Acidity = d$Acidity, Body = d$Body,
        stringsAsFactors = FALSE, check.names = FALSE)
      datatable(tab, rownames = FALSE, filter = "top",
                options = list(pageLength = 10, order = list()),
                class = "stripe hover compact")
    })
  })
}
