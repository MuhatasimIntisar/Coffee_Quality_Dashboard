# Attributing Factors tab — what shapes a great cup
# --------------------------------------------------
# Three sections that step up gently in depth (no difficulty labels, the
# progression speaks for itself):
#   1. One simple bar chart: does preparation change the score?
#   2. Two correlation views: altitude and moisture against the score
#   3. A two-factor heatmap: altitude and moisture together (blue scale)
# Every chart carries a mandatory plain-English explainer directly beneath it:
# what you are looking at, the highest peak, the lowest dip, and the takeaway.
# All peak/dip figures are computed live from the current selection.

library(bslib)

# Monochromatic blue fill for the heatmap: light and airy up to deep navy.
blue_fill <- function(name = waiver()) {
  scale_fill_gradientn(
    colours  = c("#EAF2F9", "#C9DDEE", "#9DC0DE", "#6B9CC7", "#3D6FA5", "#12395E"),
    na.value = "#EAE0D0", name = name)
}

# The mandatory explainer block that sits under every single chart.
explain_block <- function(what, peak, dip, takeaway) {
  xrow <- function(tag, txt) div(class = "xrow",
                                 span(class = "xtag", tag),
                                 span(class = "xtxt", txt))
  div(class = "explain",
      xrow("What this shows", what),
      xrow("Highest peak",    peak),
      xrow("Lowest dip",      dip),
      xrow("The takeaway",    takeaway))
}

analysisUI <- function(id) {
  ns <- NS(id)
  tagList(
    h2("What shapes a great cup"),
    p(style = "max-width:860px; font-size:17px; color:#444; line-height:1.6;",
      "Coffee quality is not luck. Where a coffee grows, how wet its beans are ",
      "and how they are prepared all leave fingerprints on the final score. This ",
      "page walks you up three steps, from a chart anyone can read at a glance ",
      "to one that rewards a closer look. Under every chart you will find a ",
      "plain-English guide to what it means."),

    # ── Section 1: the at-a-glance read ─────────────────────────────────────
    h4("Does preparation change the taste?"),
    card(full_screen = TRUE,
         card_header(textOutput(ns("method_title"))),
         card_body(
           selectInput(ns("method_measure"), "Measure",
                       choices = MEASURE_CHOICES, selected = "Total.Cup.Points"),
           plotOutput(ns("method_bar"), height = "320px"),
           uiOutput(ns("method_explain")))),

    hr(),

    # ── Section 2: a closer look ────────────────────────────────────────────
    h4("Growing conditions, one at a time"),
    layout_columns(
      col_widths = c(6, 6),
      card(full_screen = TRUE,
           card_header(textOutput(ns("alt_title"))),
           card_body(
             selectInput(ns("alt_measure"), "Measure",
                         choices = MEASURE_CHOICES, selected = "Total.Cup.Points"),
             plotOutput(ns("alt_scatter"), height = "320px"),
             uiOutput(ns("alt_explain")))),
      card(full_screen = TRUE,
           card_header(textOutput(ns("moist_title"))),
           card_body(
             selectInput(ns("moist_measure"), "Measure",
                         choices = MEASURE_CHOICES, selected = "Total.Cup.Points"),
             plotOutput(ns("moist_scatter"), height = "320px"),
             uiOutput(ns("moist_explain"))))),

    hr(),

    # ── Section 3: the deep end ─────────────────────────────────────────────
    h4("When two factors meet"),
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
           uiOutput(ns("am_explain")))),

    hr(),
    p(style = "font-size:17px; color:#2B2018; max-width:860px;",
      "Growing conditions set the ceiling; the ", strong("Sensory Analysis"),
      " tab examines the tasting attributes that determine the final grade.")
  )
}

analysisServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {

    # Populate the interaction heatmap's method checkboxes (all ticked to start).
    methods <- sort(unique(data$Processing.Method[data$Processing.Method != ""]))
    updateCheckboxGroupInput(session, "am_methods", choices = methods, selected = methods)

    # All scored coffees — this tab isn't scoped by any filter.
    base <- reactive(data[!is.na(data$Total.Cup.Points) & data$Total.Cup.Points > 0, ])

    # ── Level 1: average score per processing method, as simple bars ──────────
    method_stats <- reactive({
      m <- input$method_measure
      d <- base(); d <- d[d$Processing.Method != "" & !is.na(d[[m]]), ]
      if (m == "Total.Cup.Points") d <- d[d[[m]] > 0, ]
      keep <- names(which(table(d$Processing.Method) >= 5))
      d <- d[d$Processing.Method %in% keep, ]
      if (nrow(d) == 0) return(NULL)
      ag <- aggregate(d[[m]], list(method = d$Processing.Method), mean, na.rm = TRUE)
      names(ag) <- c("method", "value")
      ag$n <- as.integer(table(d$Processing.Method)[ag$method])
      ag[order(-ag$value), ]
    })

    output$method_title <- renderText(
      sprintf("Average %s for each preparation style",
              tolower(measure_label(input$method_measure))))

    output$method_bar <- renderPlot({
      ms <- method_stats()
      if (is.null(ms)) return(gg_no_data("Not enough data for this measure."))
      ms$method <- factor(ms$method, levels = rev(ms$method))
      lo <- floor(min(ms$value) - 1)
      # One refined hue per method (fixed palette order, no legend needed:
      # the axis names each bar), so the styles read apart without clashing.
      pal <- setNames(cat_cols(nlevels(ms$method)), levels(ms$method))
      ggplot(ms, aes(value, method, fill = method)) +
        geom_col(width = 0.62) +
        geom_text(aes(label = sprintf("%.1f", value)), hjust = -0.25,
                  size = 4.4, colour = LATTE$text, fontface = "bold") +
        scale_fill_manual(values = pal, guide = "none") +
        coord_cartesian(xlim = c(lo, max(ms$value) * 1.03)) +
        labs(x = measure_label(input$method_measure), y = NULL) +
        theme_coffee()
    })

    output$method_explain <- renderUI({
      ms <- method_stats(); req(ms)
      lab <- tolower(measure_label(input$method_measure))
      top <- ms[1, ]; low <- ms[nrow(ms), ]
      explain_block(
        sprintf("Each bar is one way of preparing coffee beans after picking, and its length is the average %s of every coffee prepared that way (only styles with at least 5 coffees are shown).", lab),
        sprintf("%s leads with an average of %.1f, across %d coffees.",
                top$method, top$value, top$n),
        sprintf("%s sits lowest at %.1f, across %d coffees.",
                low$method, low$value, low$n),
        sprintf("The gap between the best and the rest is just %.1f points, so how the beans are prepared nudges the taste rather than transforms it. Your choice of origin matters more than the label on the process.",
                top$value - low$value))
    })

    # ── Level 2a: altitude vs score ────────────────────────────────────────────
    alt_data <- reactive({
      m <- input$alt_measure
      d <- base()
      d <- d[!is.na(d$altitude_mean_meters) & d$altitude_mean_meters > 0 &
             d$altitude_mean_meters < 4000 & !is.na(d[[m]]), ]
      if (m == "Total.Cup.Points") d <- d[d[[m]] > 0, ]
      d
    })

    output$alt_title <- renderText(
      sprintf("%s against growing altitude", measure_label(input$alt_measure)))
    output$alt_scatter <- renderPlot({
      m <- input$alt_measure; d <- alt_data()
      if (nrow(d) < 3) return(gg_no_data("Not enough data for this measure."))
      ggplot(d, aes(altitude_mean_meters, .data[[m]])) +
        geom_point(colour = COFFEE_COLS$blue, alpha = 0.35, size = 1.8) +
        geom_smooth(method = "lm", se = TRUE, colour = COFFEE_COLS$green,
                    fill = COFFEE_COLS$green, alpha = 0.15) +
        labs(x = "Altitude (m)", y = measure_label(m)) + theme_coffee()
    })

    # Peak/dip for the scatter: compare altitude bands so the words match a
    # pattern the eye can actually see in the cloud of points.
    output$alt_explain <- renderUI({
      m <- input$alt_measure; d <- alt_data(); req(nrow(d) >= 3)
      lab <- tolower(measure_label(m))
      d$band <- cut(d$altitude_mean_meters, c(0, 1000, 1500, 2000, Inf),
                    c("below 1000 m", "1000 to 1500 m", "1500 to 2000 m", "above 2000 m"))
      bm <- tapply(d[[m]], d$band, mean, na.rm = TRUE); bm <- bm[!is.na(bm)]
      r  <- suppressWarnings(cor(d$altitude_mean_meters, d[[m]]))
      trend <- if (is.na(r)) "no clear direction" else
               if (r > 0.08) "a gentle climb: higher farms tend to earn higher marks" else
               if (r < -0.08) "a gentle slide: higher farms tend to earn lower marks" else
               "an almost flat line: altitude barely moves this measure"
      explain_block(
        sprintf("Every dot is one coffee, placed by the altitude of its farm (left to right) and its %s (bottom to top). The green line traces the overall trend.", lab),
        sprintf("Coffees grown %s average the most, at %.1f.",
                names(which.max(bm)), max(bm)),
        sprintf("Coffees grown %s average the least, at %.1f.",
                names(which.min(bm)), min(bm)),
        sprintf("The trend line shows %s. Thin mountain air slows the cherry's ripening, which concentrates its sugars and flavour.", trend))
    })

    # ── Level 2b: moisture vs score ────────────────────────────────────────────
    moist_data <- reactive({
      m <- input$moist_measure
      d <- base(); d <- d[!is.na(d$Moisture) & d$Moisture > 0 & !is.na(d[[m]]), ]
      if (m == "Total.Cup.Points") d <- d[d[[m]] > 0, ]
      d
    })

    output$moist_title <- renderText(
      sprintf("%s against bean moisture", measure_label(input$moist_measure)))
    output$moist_scatter <- renderPlot({
      m <- input$moist_measure; d <- moist_data()
      if (nrow(d) < 3) return(gg_no_data("Not enough data for this measure."))
      ggplot(d, aes(Moisture * 100, .data[[m]])) +
        geom_point(colour = COFFEE_COLS$blue, alpha = 0.35, size = 1.8) +
        geom_smooth(method = "lm", se = TRUE, colour = COFFEE_COLS$green,
                    fill = COFFEE_COLS$green, alpha = 0.15) +
        labs(x = "Moisture (%)", y = measure_label(m)) + theme_coffee()
    })

    output$moist_explain <- renderUI({
      m <- input$moist_measure; d <- moist_data(); req(nrow(d) >= 3)
      lab <- tolower(measure_label(m))
      d$band <- cut(d$Moisture, c(0, 0.10, 0.12, Inf),
                    c("drier than 10%", "between 10% and 12%", "wetter than 12%"))
      bm <- tapply(d[[m]], d$band, mean, na.rm = TRUE); bm <- bm[!is.na(bm)]
      r  <- suppressWarnings(cor(d$Moisture, d[[m]]))
      trend <- if (is.na(r)) "no clear direction" else
               if (r > 0.08) "wetter beans tending to score a little higher" else
               if (r < -0.08) "wetter beans tending to score a little lower" else
               "moisture making almost no difference on its own"
      explain_block(
        sprintf("Every dot is one coffee, placed by how much moisture its beans held at grading (left to right) and its %s (bottom to top).", lab),
        sprintf("Beans %s average the most, at %.1f.", names(which.max(bm)), max(bm)),
        sprintf("Beans %s average the least, at %.1f.", names(which.min(bm)), min(bm)),
        sprintf("The trend shows %s. Moisture matters most in combination with altitude, which is exactly what the chart below explores.", trend))
    })

    # ── Level 3: altitude and moisture together ────────────────────────────────
    am_cells <- reactive({
      m <- input$am_measure
      sel <- input$am_methods
      if (is.null(sel) || length(sel) == 0) return(NULL)
      d <- base(); d <- d[!is.na(d[[m]]) & d$Processing.Method %in% sel, ]
      if (m == "Total.Cup.Points") d <- d[d[[m]] > 0, ]
      d <- d[!is.na(d$altitude_mean_meters) & d$altitude_mean_meters > 0 &
             d$altitude_mean_meters < 4000 &
             !is.na(d$Moisture) & d$Moisture > 0, ]
      if (nrow(d) == 0) return(NULL)
      d$altband <- cut(d$altitude_mean_meters,
                       c(0, 1000, 1250, 1500, 1750, 2000, Inf),
                       c("<1000", "1000–1250", "1250–1500", "1500–1750",
                         "1750–2000", "2000+"), right = FALSE)
      d$moband  <- cut(d$Moisture, c(0, 0.10, 0.11, 0.12, 0.13, Inf),
                       c("<10%", "10–11%", "11–12%", "12–13%", "13%+"), right = FALSE)
      ag <- aggregate(d[[m]], list(alt = d$altband, mo = d$moband), mean, na.rm = TRUE)
      names(ag) <- c("alt", "mo", "val")
      ag
    })

    output$am_title <- renderText(
      sprintf("Average %s across altitude and moisture together",
              tolower(measure_label(input$am_measure))))
    output$altmoist <- renderPlot({
      ag <- am_cells()
      if (is.null(ag)) {
        if (is.null(input$am_methods) || length(input$am_methods) == 0)
          return(gg_no_data("Tick at least one processing method."))
        return(gg_no_data("No coffees match this selection."))
      }
      mid <- mean(range(ag$val))
      ggplot(ag, aes(alt, mo, fill = val)) +
        geom_tile(colour = "white") +
        geom_text(aes(label = sprintf("%.1f", val), colour = val > mid),
                  size = 3, show.legend = FALSE) +
        scale_colour_manual(values = c(`TRUE` = "white", `FALSE` = "#222222")) +
        blue_fill(measure_label(input$am_measure)) +
        labs(x = "Altitude band (m)", y = "Moisture") + theme_coffee() +
        theme(axis.text.x = element_text(angle = 25, hjust = 1))
    })

    output$am_explain <- renderUI({
      ag <- am_cells(); req(ag)
      lab <- tolower(measure_label(input$am_measure))
      top <- ag[which.max(ag$val), ]; low <- ag[which.min(ag$val), ]
      explain_block(
        sprintf("A grid of every altitude and moisture combination. Each cell's colour is the average %s of the coffees inside it: the deeper the blue, the higher the score. Blank cells simply have no coffees.", lab),
        sprintf("The sweet spot is the %s m altitude band at %s moisture, averaging %.1f.",
                top$alt, top$mo, top$val),
        sprintf("The weakest cell is the %s m band at %s moisture, averaging %.1f.",
                low$alt, low$mo, low$val),
        "Neither factor rules alone. The best cups come from the right pairing of mountain height and bean moisture, so a grower chasing quality has two dials to tune, not one.")
    })
  })
}
