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
    h2("Attributing Factors — how growing & processing affect the score"),
    p("Each section isolates one growing-or-processing factor and shows how it ",
      "relates to the score: the processing method, altitude, and moisture — then ",
      "how altitude and moisture interact. (Which tasting notes drive the score is ",
      "on the Sensory Analysis tab.)"),

    accordion(
      open = "Method — does processing change the score?",

        # ── 1. Method ──────────────────────────────────────────────────────────
        accordion_panel(
          "Method — does processing change the score?",
          card(full_screen = TRUE,
               card_header(textOutput(ns("method_title"))),
               card_body(
                 selectInput(ns("method_measure"), "Measure",
                             choices = MEASURE_CHOICES, selected = "Total.Cup.Points"),
                 plotOutput(ns("method_box"), height = "340px"),
                 p(class = "card-note",
                   "A box per processing method (methods with at least 5 coffees) — ",
                   "the box spans the middle half of scores, the line is the median. ",
                   "Higher boxes score better.")))
        ),

        # ── 2. Altitude & moisture, side by side ───────────────────────────────
        accordion_panel(
          "Altitude & moisture",
          layout_columns(
            col_widths = c(6, 6),
            card(full_screen = TRUE,
                 card_header(textOutput(ns("alt_title"))),
                 card_body(
                   selectInput(ns("alt_measure"), "Measure",
                               choices = MEASURE_CHOICES, selected = "Total.Cup.Points"),
                   plotOutput(ns("alt_scatter"), height = "340px"),
                   p(class = "card-note",
                     "Each point is a coffee: altitude vs the chosen measure, with a ",
                     "trend line. The subtitle sums up the direction."))),
            card(full_screen = TRUE,
                 card_header(textOutput(ns("moist_title"))),
                 card_body(
                   selectInput(ns("moist_measure"), "Measure",
                               choices = MEASURE_CHOICES, selected = "Total.Cup.Points"),
                   plotOutput(ns("moist_scatter"), height = "340px"),
                   p(class = "card-note",
                     "Each point is a coffee: moisture vs the chosen measure, with a ",
                     "trend line. The subtitle sums up the direction."))))
        ),

        # ── 3. Interaction ─────────────────────────────────────────────────────
        accordion_panel(
          "How altitude & moisture interact",
          card(full_screen = TRUE,
               card_header(textOutput(ns("am_title"))),
               card_body(
                 layout_columns(
                   col_widths = c(8, 4),
                   checkboxGroupInput(ns("am_methods"), "Processing methods",
                                      choices = character(0), inline = TRUE),
                   selectInput(ns("am_measure"), "Fill by",
                               choices = MEASURE_CHOICES, selected = "Total.Cup.Points")),
                 plotOutput(ns("altmoist"), height = "380px"),
                 p(class = "card-note",
                   "Mean of the chosen measure in each altitude × moisture cell — ",
                   "brighter is higher. Tick which processing methods to include. ",
                   "Blank cells have no coffees.")))
        )
      ),

    hr(),
    p(style = "font-size:17px; color:#2B2018; max-width:860px;",
      "That's the growing & processing side. Next: which ", strong("tasting notes"),
      " actually drive the score — head to the ", strong("Sensory Analysis"), " tab.")
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
      meds <- tapply(d[[m]], d$Processing.Method, median, na.rm = TRUE)
      sub  <- sprintf("%s scores highest — but the gap between methods is modest.",
                      names(meds)[which.max(meds)])
      ggplot(d, aes(reorder(Processing.Method, .data[[m]], FUN = median),
                    .data[[m]], fill = Processing.Method)) +
        geom_boxplot(alpha = 0.85, width = 0.5, outlier.size = 0.7,
                     outlier.alpha = 0.4, linewidth = 0.4) +
        scale_fill_manual(values = cat_cols(length(unique(d$Processing.Method))),
                          guide = "none") +
        labs(x = NULL, y = measure_label(m), subtitle = sub) +
        theme_coffee() + coord_flip()
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
      r <- suppressWarnings(cor(d$altitude_mean_meters, d[[m]]))
      sub <- if (is.na(r)) "" else if (r > 0.08) "Higher-grown coffees tend to score higher" else
             if (r < -0.08) "Higher-grown coffees tend to score lower" else
             "Little relationship between altitude and this score"
      ggplot(d, aes(altitude_mean_meters, .data[[m]])) +
        geom_point(colour = COFFEE_COLS$blue, alpha = 0.35, size = 1.8) +
        geom_smooth(method = "lm", se = TRUE, colour = COFFEE_COLS$green,
                    fill = COFFEE_COLS$green, alpha = 0.15) +
        labs(x = "Altitude (m)", y = measure_label(m), subtitle = sub) + theme_coffee()
    })

    # ── 2. Moisture (paired with altitude): score vs moisture ───────────────────
    output$moist_title <- renderText(
      sprintf("%s vs moisture", measure_label(input$moist_measure)))
    output$moist_scatter <- renderPlot({
      m <- input$moist_measure
      d <- base(); d <- d[!is.na(d$Moisture) & d$Moisture > 0 & !is.na(d[[m]]), ]
      if (m == "Total.Cup.Points") d <- d[d[[m]] > 0, ]
      if (nrow(d) < 3) return(gg_no_data("Not enough data for this slice."))
      r <- suppressWarnings(cor(d$Moisture, d[[m]]))
      sub <- if (is.na(r)) "" else if (r > 0.08) "Wetter beans tend to score higher" else
             if (r < -0.08) "Wetter beans tend to score lower" else
             "Little relationship between moisture and this score"
      ggplot(d, aes(Moisture * 100, .data[[m]])) +
        geom_point(colour = COFFEE_COLS$blue, alpha = 0.35, size = 1.8) +
        geom_smooth(method = "lm", se = TRUE, colour = COFFEE_COLS$green,
                    fill = COFFEE_COLS$green, alpha = 0.15) +
        labs(x = "Moisture (%)", y = measure_label(m), subtitle = sub) + theme_coffee()
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
      d$altband <- cut(d$altitude_mean_meters,
                       c(0, 1000, 1250, 1500, 1750, 2000, Inf),
                       c("<1000", "1000–1250", "1250–1500", "1500–1750",
                         "1750–2000", "2000+"), right = FALSE)
      d$moband  <- cut(d$Moisture, c(0, 0.10, 0.11, 0.12, 0.13, Inf),
                       c("<10%", "10–11%", "11–12%", "12–13%", "13%+"), right = FALSE)
      ag <- aggregate(d[[m]], list(alt = d$altband, mo = d$moband), mean, na.rm = TRUE)
      names(ag) <- c("alt", "mo", "val")
      mid <- mean(range(ag$val))
      top <- ag[which.max(ag$val), ]
      sub <- sprintf("Highest in the %s m band at %s moisture.",
                     as.character(top$alt), as.character(top$mo))
      ggplot(ag, aes(alt, mo, fill = val)) +
        geom_tile(colour = "white") +
        geom_text(aes(label = sprintf("%.1f", val), colour = val > mid),
                  size = 3, show.legend = FALSE) +
        scale_colour_manual(values = c(`TRUE` = "white", `FALSE` = "#222222")) +
        warm_fill(measure_label(m)) +
        labs(x = "Altitude band (m)", y = "Moisture", subtitle = sub) + theme_coffee() +
        theme(axis.text.x = element_text(angle = 25, hjust = 1))
    })
  })
}
