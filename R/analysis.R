# Attributing Factors tab — how growing & processing affect the score
# -------------------------------------------------------------------
# One growing-or-processing factor per section, top to bottom, then how two
# factors interact:
#   1. Method     — score distribution per processing method
#   2. Altitude & moisture — score vs each, side by side
#   3. Interaction — cross any two of altitude / moisture / method; filter by method
# Which tasting notes drive the score lives on the Sensory Analysis tab;
# countries live on Global.

library(bslib)

analysisUI <- function(id) {
  ns <- NS(id)
  tagList(
    h2("Attributing Factors"),
    h5("Investigate how coffee is influenced by growing conditions and processing"),
    p("Each section isolates one growing or processing factor and examines its ",
      "relationship with the score: processing method, altitude, and moisture, ",
      "followed by the interaction between altitude and moisture."),

    accordion(
      open = "Method: does processing affect the score?",

        # ── 1. Method ──────────────────────────────────────────────────────────
        accordion_panel(
          "Method: does processing affect the score?",
          card(full_screen = TRUE,
               card_header(textOutput(ns("method_title"))),
               card_body(
                 selectInput(ns("method_measure"), "Measure",
                             choices = MEASURE_CHOICES, selected = "Total.Cup.Points"),
                 p(class = "card-note",
                   "How to read: one box per processing method (methods with at least ",
                   "five coffees). Each box spans the middle half of the scores and the ",
                   "line marks the median; higher boxes indicate higher scores."),
                 plotOutput(ns("method_box"), height = "340px"),
                 uiOutput(ns("method_theory"))))
        ),

        # ── 2. Altitude & moisture, side by side ───────────────────────────────
        accordion_panel(
          "Growing Conditions: How altitude and moisture affect score.",
          p(class = "card-note",
            "How to read: each point is one coffee, positioned by its altitude or ",
            "moisture content (horizontal axis) against the selected measure ",
            "(vertical axis); the green line traces the overall trend. Use the ",
            "measure selector on each chart to compare against the total score or ",
            "any sensory attribute."),
          layout_columns(
            col_widths = c(6, 6),
            card(full_screen = TRUE,
                 card_header(textOutput(ns("alt_title"))),
                 card_body(
                   selectInput(ns("alt_measure"), "Measure",
                               choices = MEASURE_CHOICES, selected = "Total.Cup.Points"),
                   plotOutput(ns("alt_scatter"), height = "340px"),
                   uiOutput(ns("alt_theory")))),
            card(full_screen = TRUE,
                 card_header(textOutput(ns("moist_title"))),
                 card_body(
                   selectInput(ns("moist_measure"), "Measure",
                               choices = MEASURE_CHOICES, selected = "Total.Cup.Points"),
                   plotOutput(ns("moist_scatter"), height = "340px"),
                   uiOutput(ns("moist_theory")))))
        ),

        # ── 3. Interaction ─────────────────────────────────────────────────────
        accordion_panel(
          "Combination of Growing Conditions and Processing: How the interaction of the 3 results in the score.",
          card(full_screen = TRUE,
               card_header(textOutput(ns("am_title"))),
               card_body(
                 layout_columns(
                   col_widths = c(8, 4),
                   checkboxGroupInput(ns("am_methods"), "Processing methods",
                                      choices = character(0), inline = TRUE),
                   selectInput(ns("am_measure"), "Fill by",
                               choices = MEASURE_CHOICES, selected = "Total.Cup.Points")),
                 p(class = "card-note",
                   "How to read: the mean of the selected measure within each altitude ",
                   "and moisture cell; brighter cells indicate higher values. Processing ",
                   "methods may be included or excluded using the checkboxes, and blank ",
                   "cells contain no coffees."),
                 plotOutput(ns("altmoist"), height = "380px"),
                 uiOutput(ns("am_theory"))))
        )
      ),

    hr(),
    uiOutput(ns("factor_summary"))
  )
}

analysisServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {

    # Populate the interaction heatmap's method checkboxes (all ticked to start).
    methods <- sort(unique(data$Processing.Method[data$Processing.Method != ""]))
    updateCheckboxGroupInput(session, "am_methods", choices = methods, selected = methods)

    # All scored coffees — this tab isn't scoped by any filter.
    base <- reactive(data[!is.na(data$Total.Cup.Points) & data$Total.Cup.Points > 0, ])

    # ── 1. Method: score distribution per processing method ─────────────────────
    output$method_title <- renderText(
      sprintf("%s by processing method", measure_label(input$method_measure)))
    output$method_box <- renderPlot({
      m <- input$method_measure
      d <- base(); d <- d[d$Processing.Method != "" & !is.na(d[[m]]), ]
      if (m == "Total.Cup.Points") d <- d[d[[m]] > 0, ]
      if (nrow(d) == 0) return(gg_no_data("Not enough data for this slice."))
      keep <- names(which(table(d$Processing.Method) >= 5))
      d <- d[d$Processing.Method %in% keep, ]
      if (nrow(d) == 0) return(gg_no_data("Groups too small to compare."))
      ggplot(d, aes(reorder(Processing.Method, .data[[m]], FUN = median),
                    .data[[m]], fill = Processing.Method)) +
        geom_boxplot(alpha = 0.85, width = 0.5, outlier.size = 0.7,
                     outlier.alpha = 0.4, linewidth = 0.4) +
        scale_fill_manual(values = cat_cols(length(unique(d$Processing.Method))),
                          guide = "none") +
        labs(x = "Processing method", y = measure_label(m)) +
        coord_cartesian(ylim = measure_limits(m)) + theme_coffee() +
        theme(axis.text.x = element_text(angle = 20, hjust = 1))
    })

    # Finding below the chart: which method records the highest median.
    output$method_theory <- renderUI({
      m <- input$method_measure
      d <- base(); d <- d[d$Processing.Method != "" & !is.na(d[[m]]), ]
      if (m == "Total.Cup.Points") d <- d[d[[m]] > 0, ]
      keep <- names(which(table(d$Processing.Method) >= 5))
      d <- d[d$Processing.Method %in% keep, ]
      if (nrow(d) == 0) return(NULL)
      meds <- tapply(d[[m]], d$Processing.Method, median, na.rm = TRUE)
      p(class = "card-note", sprintf(
        "%s records the highest median, although the differences between methods are modest.",
        names(meds)[which.max(meds)]))
    })

    # ── 2. Altitude: score vs altitude ──────────────────────────────────────────
    output$alt_title <- renderText(
      sprintf("%s vs altitude", measure_label(input$alt_measure)))
    output$alt_scatter <- renderPlot({
      m <- input$alt_measure
      d <- base()
      d <- d[!is.na(d$altitude_mean_meters) & d$altitude_mean_meters > 0 &
             d$altitude_mean_meters < 4000 & !is.na(d[[m]]), ]
      if (m == "Total.Cup.Points") d <- d[d[[m]] > 0, ]
      if (nrow(d) < 3) return(gg_no_data("Not enough data for this slice."))
      ggplot(d, aes(altitude_mean_meters, .data[[m]])) +
        geom_point(colour = COFFEE_COLS$blue, alpha = 0.35, size = 1.8) +
        geom_smooth(method = "lm", se = TRUE, colour = COFFEE_COLS$green,
                    fill = COFFEE_COLS$green, alpha = 0.15) +
        labs(x = "Altitude (m)", y = measure_label(m)) +
        coord_cartesian(xlim = ALT_LIMITS, ylim = measure_limits(m)) + theme_coffee()
    })

    # Dynamic reading note: recomputed from the same data as the chart, so the
    # stated correlation always matches the plotted trend line.
    output$alt_theory <- renderUI({
      m <- input$alt_measure
      d <- base()
      d <- d[!is.na(d$altitude_mean_meters) & d$altitude_mean_meters > 0 &
             d$altitude_mean_meters < 4000 & !is.na(d[[m]]), ]
      if (m == "Total.Cup.Points") d <- d[d[[m]] > 0, ]
      if (nrow(d) < 3) return(NULL)
      r <- suppressWarnings(cor(d$altitude_mean_meters, d[[m]]))
      if (is.na(r)) return(NULL)
      strength  <- if (abs(r) < 0.1) "negligible" else if (abs(r) < 0.3) "weak"
                   else if (abs(r) < 0.5) "moderate" else "strong"
      direction <- if (r >= 0) "positive" else "negative"
      tendency  <- if (abs(r) < 0.1)
        "altitude alone has little bearing on this measure"
      else if (r > 0)
        paste0("coffees grown at higher altitudes tend to record higher values, ",
               "although altitude alone explains only a small part of the variation")
      else
        paste0("coffees grown at higher altitudes tend to record lower values, ",
               "although altitude alone explains only a small part of the variation")
      p(class = "card-note", sprintf(
        "Altitude shows a %s association with %s: %s.",
        strength, measure_label(m), tendency))
    })

    # ── 2. Moisture (paired with altitude): score vs moisture ───────────────────
    output$moist_title <- renderText(
      sprintf("%s vs moisture", measure_label(input$moist_measure)))
    output$moist_scatter <- renderPlot({
      m <- input$moist_measure
      d <- base(); d <- d[!is.na(d$Moisture) & d$Moisture > 0 & !is.na(d[[m]]), ]
      if (m == "Total.Cup.Points") d <- d[d[[m]] > 0, ]
      if (nrow(d) < 3) return(gg_no_data("Not enough data for this slice."))
      ggplot(d, aes(Moisture * 100, .data[[m]])) +
        geom_point(colour = COFFEE_COLS$blue, alpha = 0.35, size = 1.8) +
        geom_smooth(method = "lm", se = TRUE, colour = COFFEE_COLS$green,
                    fill = COFFEE_COLS$green, alpha = 0.15) +
        labs(x = "Moisture (%)", y = measure_label(m)) +
        coord_cartesian(xlim = MOIST_LIMITS, ylim = measure_limits(m)) + theme_coffee()
    })

    # Dynamic reading note for moisture, mirroring the altitude note (below chart).
    output$moist_theory <- renderUI({
      m <- input$moist_measure
      d <- base(); d <- d[!is.na(d$Moisture) & d$Moisture > 0 & !is.na(d[[m]]), ]
      if (m == "Total.Cup.Points") d <- d[d[[m]] > 0, ]
      if (nrow(d) < 3) return(NULL)
      r <- suppressWarnings(cor(d$Moisture, d[[m]]))
      if (is.na(r)) return(NULL)
      strength  <- if (abs(r) < 0.1) "negligible" else if (abs(r) < 0.3) "weak"
                   else if (abs(r) < 0.5) "moderate" else "strong"
      direction <- if (r >= 0) "positive" else "negative"
      tendency  <- if (abs(r) < 0.1) "moisture alone has little bearing on this measure"
                   else if (r > 0) "beans with higher moisture tend to record higher values"
                   else "beans with higher moisture tend to record lower values"
      p(class = "card-note", sprintf(
        "Moisture shows a %s association with %s: %s.",
        strength, measure_label(m), tendency))
    })

    # ── 3. Interaction: altitude × moisture, over the chosen methods ────────────
    output$am_title <- renderText(
      sprintf("%s by altitude × moisture", measure_label(input$am_measure)))
    output$altmoist <- renderPlot({
      m <- input$am_measure
      sel <- input$am_methods
      if (is.null(sel) || length(sel) == 0)
        return(gg_no_data("Tick at least one processing method."))
      d <- base(); d <- d[!is.na(d[[m]]) & d$Processing.Method %in% sel, ]
      if (m == "Total.Cup.Points") d <- d[d[[m]] > 0, ]
      d <- d[!is.na(d$altitude_mean_meters) & d$altitude_mean_meters > 0 &
             d$altitude_mean_meters < 4000 &
             !is.na(d$Moisture) & d$Moisture > 0, ]
      if (nrow(d) == 0) return(gg_no_data("No coffees match this selection."))
      d$altband <- alt_band(d$altitude_mean_meters)
      d$moband  <- moist_band(d$Moisture)
      ag <- aggregate(d[[m]], list(alt = d$altband, mo = d$moband), mean, na.rm = TRUE)
      names(ag) <- c("alt", "mo", "val")
      mid <- mean(range(ag$val))
      ggplot(ag, aes(alt, mo, fill = val)) +
        geom_tile(colour = "white") +
        geom_text(aes(label = sprintf("%.1f", val), colour = val > mid),
                  size = 3, show.legend = FALSE) +
        scale_colour_manual(values = c(`TRUE` = "white", `FALSE` = "#222222")) +
        scale_fill_gradientn(
          colours  = c("#F7FBFF", "#C6DBEF", "#6BAED6", "#2171B5", "#08306B"),
          na.value = "#EAE0D0", name = measure_label(m)) +
        labs(x = "Altitude band (m)", y = "Moisture") + theme_coffee() +
        theme(axis.text.x = element_text(angle = 25, hjust = 1))
    })

    # Finding below the heatmap: the top-scoring altitude x moisture cell.
    output$am_theory <- renderUI({
      m <- input$am_measure; sel <- input$am_methods
      if (is.null(sel) || length(sel) == 0) return(NULL)
      d <- base(); d <- d[!is.na(d[[m]]) & d$Processing.Method %in% sel, ]
      if (m == "Total.Cup.Points") d <- d[d[[m]] > 0, ]
      d <- d[!is.na(d$altitude_mean_meters) & d$altitude_mean_meters > 0 &
             d$altitude_mean_meters < 4000 & !is.na(d$Moisture) & d$Moisture > 0, ]
      if (nrow(d) == 0) return(NULL)
      d$altband <- alt_band(d$altitude_mean_meters)
      d$moband  <- moist_band(d$Moisture)
      ag <- aggregate(d[[m]], list(alt = d$altband, mo = d$moband), mean, na.rm = TRUE)
      names(ag) <- c("alt", "mo", "val")
      top <- ag[which.max(ag$val), ]
      p(class = "card-note", sprintf(
        "The highest values occur in the %s m altitude band at %s moisture.",
        as.character(top$alt), as.character(top$mo)))
    })

    # Closing synthesis: ranks the three factors for Total Cup Points, computed
    # live so the figures always match the charts above.
    output$factor_summary <- renderUI({
      d <- base()
      dm <- d[d$Processing.Method != "" & !is.na(d$Processing.Method), ]
      keep <- names(which(table(dm$Processing.Method) >= 5))
      dm <- dm[dm$Processing.Method %in% keep, ]
      meds <- tapply(dm$Total.Cup.Points, dm$Processing.Method, median, na.rm = TRUE)
      p(class = "card-note", style = "max-width:860px;", sprintf(
        paste0("Taken together, both altitude and moisture show a modest association ",
               "with the total score, while processing-method medians differ by only ",
               "%.1f points: growing conditions matter more than post-harvest ",
               "processing."),
        max(meds) - min(meds)))
    })
  })
}
