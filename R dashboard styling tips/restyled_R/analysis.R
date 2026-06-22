# Analysis tab  — bslib sidebar + card layout
# -------------------------------------------
# Controls are pinned in a left sidebar; each plot lives in its own titled card
# (with a full-screen expand button). Server logic is UNCHANGED from the
# original — only analysisUI() was rewritten. This file is the reference
# pattern; apply the same layout_sidebar + card structure to the other tabs.

library(DT)
library(bslib)

# Scatter x-axis options (continuous factors) and heatmap layouts.
XVAR_CHOICES <- c("Altitude (m)" = "altitude_mean_meters",
                  "Moisture (%)" = "Moisture")
HEAT_CHOICES <- c("Altitude \u00D7 Moisture" = "alt_moist",
                  "Altitude \u00D7 Method"   = "alt_method")

analysisUI <- function(id) {
  ns <- NS(id)
  tagList(
    h2("Analysis: trends, factors & finder"),
    p("Explore how where and how coffee is grown relates to quality and flavour. ",
      "Filter to a slice, pick a measure to study, then read it across time, ",
      "against altitude or moisture, and as a factor heatmap. Use the finder at ",
      "the bottom to pull out the specific coffees that match a category."),

    layout_sidebar(
      fillable = FALSE,

      # ── Filters live in the sidebar ───────────────────────────────────────
      sidebar = sidebar(
        title = "Filters",
        width = 300,
        selectInput(ns("country"), "Country",           choices = "All countries"),
        selectInput(ns("method"),  "Processing method", choices = "All methods"),
        sliderInput(ns("years"), "Harvest year range",
                    min = YEAR_RANGE[1], max = YEAR_RANGE[2],
                    value = YEAR_RANGE, step = 1, sep = "", width = "100%"),
        hr(),
        selectInput(ns("measure"), "Measure to study",
                    choices = MEASURE_CHOICES, selected = "Total.Cup.Points"),
        selectInput(ns("xvar"), "Scatter: x variable",
                    choices = XVAR_CHOICES, selected = "altitude_mean_meters"),
        selectInput(ns("heat"), "Heatmap layout",
                    choices = HEAT_CHOICES, selected = "alt_moist")
      ),

      # ── Trend + scatter ───────────────────────────────────────────────────
      layout_columns(
        col_widths = c(6, 6),
        card(full_screen = TRUE,
             card_header(textOutput(ns("timeline_title"))),
             plotOutput(ns("timeline"), height = "320px")),
        card(full_screen = TRUE,
             card_header(textOutput(ns("scatter_title"))),
             plotOutput(ns("scatter"), height = "320px"))
      ),

      # ── Heatmap + flavour profile ─────────────────────────────────────────
      layout_columns(
        col_widths = c(7, 5),
        card(full_screen = TRUE,
             card_header(textOutput(ns("heat_title"))),
             card_body(
               p(class = "card-note",
                 "Mean of the chosen measure in each cell; blank cells have no coffees."),
               plotOutput(ns("heat"), height = "360px"))),
        card(full_screen = TRUE,
             card_header("Flavour profile of the slice"),
             card_body(
               p(class = "card-note", textOutput(ns("flavor_caption"), inline = TRUE)),
               plotOutput(ns("flavor"), height = "360px")))
      ),

      # ── Coffee finder ─────────────────────────────────────────────────────
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

    mlab <- reactive(measure_label(input$measure))

    # Scored coffees passing the country / method / year filters.
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
          c("<1000", "1000\u20131250", "1250\u20131500", "1500\u20131750", "1750\u20132000", "2000+"),
          right = FALSE)
    }

    # ── Timeline: mean measure per harvest year ─────────────────────────────────
    output$timeline_title <- renderText(sprintf("%s over time", mlab()))
    output$timeline <- renderPlot({
      m <- input$measure
      d <- base(); d <- d[!is.na(d[[m]]), ]
      if (nrow(d) == 0) return(gg_no_data())
      ag <- aggregate(d[[m]], list(year = d$harvest_year), mean, na.rm = TRUE)
      names(ag) <- c("year", "val")
      ggplot(ag, aes(year, val)) +
        geom_line(colour = COFFEE_COLS$blue, linewidth = 1) +
        geom_point(colour = COFFEE_COLS$blue, size = 2.6) +
        scale_x_continuous(breaks = ag$year) +
        labs(x = "Harvest year", y = paste("Mean", mlab())) + theme_coffee()
    })

    # ── Scatter: altitude/moisture vs measure, coloured by method ───────────────
    output$scatter_title <- renderText({
      xl <- names(XVAR_CHOICES)[XVAR_CHOICES == input$xvar]
      sprintf("%s vs %s", mlab(), xl)
    })
    output$scatter <- renderPlot({
      m <- input$measure; xv <- input$xvar
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
        labs(x = xl, y = mlab(), colour = "Method") + theme_coffee()
    })

    # ── Heatmap: altitude band × (moisture band | method), filled by measure ────
    output$heat_title <- renderText(
      if (input$heat == "alt_method") sprintf("%s by altitude \u00D7 method", mlab())
      else sprintf("%s by altitude \u00D7 moisture", mlab()))
    output$heat <- renderPlot({
      m <- input$measure
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
                        c("<10%", "10\u201311%", "11\u201312%", "12\u201313%", "13%+"), right = FALSE)
        ag <- aggregate(d[[m]], list(x = d$altband, y = d$moband), mean, na.rm = TRUE)
        ylab <- "Moisture"
      }
      names(ag) <- c("x", "y", "val")
      ggplot(ag, aes(x, y, fill = val)) +
        geom_tile(colour = "white") +
        geom_text(aes(label = sprintf("%.1f", val)), size = 3, colour = "#333333") +
        scale_fill_gradient(low = "#F1E4CE", high = COFFEE_COLS$blue) +
        labs(x = "Altitude band (m)", y = ylab, fill = mlab()) +
        theme_coffee() + theme(axis.text.x = element_text(angle = 25, hjust = 1))
    })

    # ── Flavour profile (bar chart, not radar) + total flavour caption ──────────
    output$flavor_caption <- renderText({
      s <- base()
      if (nrow(s) == 0) return("No coffees in this slice.")
      sprintf("Mean Total Cup Points: %.1f across %d coffees.",
              mean(s$Total.Cup.Points, na.rm = TRUE), nrow(s))
    })
    output$flavor <- renderPlot({
      s <- base()
      if (nrow(s) == 0) return(gg_no_data())
      means <- sapply(FLAVOR_ATTRS, function(a) mean(s[[a]], na.rm = TRUE))
      df <- data.frame(attr = gsub("\\.", " ", FLAVOR_ATTRS), val = means,
                       stringsAsFactors = FALSE)
      df$attr <- factor(df$attr, levels = df$attr[order(df$val)])
      # Highlight the measure currently under study, if it's a sensory attribute.
      hi <- gsub("\\.", " ", input$measure)
      df$fill <- ifelse(as.character(df$attr) == hi, COFFEE_COLS$orange, COFFEE_COLS$blue)
      ggplot(df, aes(val, attr, fill = fill)) +
        geom_col(width = 0.7) +
        geom_text(aes(label = sprintf("%.2f", val)), hjust = -0.15, size = 3.2,
                  colour = "#333333") +
        scale_fill_identity() +
        scale_x_continuous(expand = expansion(mult = c(0, 0.12))) +
        labs(x = "Mean score (out of 10)", y = NULL) + theme_coffee()
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
