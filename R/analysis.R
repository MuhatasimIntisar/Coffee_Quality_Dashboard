# Analysis tab  — bslib sidebar + collapsible accordion sections
# --------------------------------------------------------------
# Global filters (country / method / years) are pinned in the left sidebar.
# The body is an accordion of three discovery sections — Flavour profile &
# quality drivers (paired side by side), Method, and Altitude × moisture — all
# expanded on load. Controls sit beneath each chart. (Ranking now lives on the
# Global tab; the Coffee finder on the Summary tab.)

library(bslib)
library(fmsb)   # radar / spider charts (replaces the hand-coded base-R radar)

# Grouping factors for the Flavour-profile radar overlay.
RADAR_GROUPS <- c("Altitude band"     = "altband",
                  "Processing method" = "method",
                  "Moisture band"     = "moband")

analysisUI <- function(id) {
  ns <- NS(id)
  tagList(
    h2("Analysis: trends, factors & drivers"),
    p("Explore how where and how coffee is grown relates to quality and flavour. ",
      "The filters on the left scope every section; each section below has its ",
      "own controls on the right."),

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

      # ── Collapsible sections ────────────────────────────────────────────────
      # accordion() stacks the sections; open = TRUE expands them all on load,
      # and the user can collapse any header to fold a section away. Controls sit
      # beneath each chart; the first section pairs two charts side by side.
      accordion(
        open = TRUE,

        # ── 1. Flavour profile + Quality drivers, side by side ─────────────────
        accordion_panel(
          "Flavour profile & quality drivers",
          layout_columns(
            col_widths = c(6, 6),
            card(full_screen = TRUE,
                 card_header(textOutput(ns("radar_title"))),
                 card_body(
                   plotOutput(ns("radar"), height = "380px"),
                   selectInput(ns("radar_group"), "Compare by",
                               choices = RADAR_GROUPS, selected = "altband"),
                   radioButtons(ns("radar_scale"), "Scale",
                                choices = c("Relative" = "rel", "Absolute" = "abs"),
                                selected = "rel", inline = TRUE),
                   p(class = "card-note",
                     "Mean score on each attribute, one polygon per group (groups ",
                     "under 5 coffees dropped). Relative zooms each axis to the ",
                     "groups' range; Absolute uses a fixed 6–10 scale."))),
            card(full_screen = TRUE,
                 card_header(textOutput(ns("driver_title"))),
                 card_body(
                   plotOutput(ns("drivers"), height = "380px"),
                   checkboxGroupInput(ns("driver_attrs"), "Sensory attributes",
                               choices = setNames(FLAVOR_ATTRS,
                                                  gsub("\\.", " ", FLAVOR_ATTRS)),
                               selected = c("Aroma", "Acidity"), inline = TRUE),
                   p(class = "card-note",
                     "Each line fits one attribute against the overall score; its r ",
                     "is in the legend — steeper, higher-r lines track quality most. ",
                     "(Point clouds show when 3 or fewer are selected.)"))))
        ),

        # ── 2. Method: a chosen measure by processing method ───────────────────
        accordion_panel(
          "Method",
          card(full_screen = TRUE,
               card_header(textOutput(ns("method_title"))),
               card_body(
                 plotOutput(ns("method_box"), height = "340px"),
                 selectInput(ns("method_measure"), "Measure",
                             choices = MEASURE_CHOICES, selected = "Total.Cup.Points"),
                 p(class = "card-note",
                   "Distribution of the chosen measure for each processing method ",
                   "(methods with at least 5 coffees in the current slice).")))
        ),

        # ── 3. Altitude × moisture: heatmap of the mean measure per band ───────
        accordion_panel(
          "Altitude × moisture",
          card(full_screen = TRUE,
               card_header(textOutput(ns("am_title"))),
               card_body(
                 plotOutput(ns("altmoist"), height = "360px"),
                 selectInput(ns("am_measure"), "Fill by",
                             choices = MEASURE_CHOICES, selected = "Total.Cup.Points"),
                 p(class = "card-note",
                   "Mean of the chosen measure in each altitude × moisture cell; ",
                   "brighter cells are higher. Blank cells have no coffees.")))
        )
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

    # ── 3. Method: distribution of a chosen measure per processing method ───────
    output$method_title <- renderText(
      sprintf("%s by processing method", measure_label(input$method_measure)))
    output$method_box <- renderPlot({
      m <- input$method_measure
      d <- base(); d <- d[d$Processing.Method != "" & !is.na(d[[m]]), ]
      if (m == "Total.Cup.Points") d <- d[d[[m]] > 0, ]
      if (nrow(d) == 0) return(gg_no_data("Not enough data for this slice."))
      # Keep methods with at least 5 coffees so a single coffee isn't a "box".
      keep <- names(which(table(d$Processing.Method) >= 5))
      d <- d[d$Processing.Method %in% keep, ]
      if (nrow(d) == 0) return(gg_no_data("Groups too small to compare."))
      ggplot(d, aes(reorder(Processing.Method, .data[[m]], FUN = median),
                    .data[[m]], fill = Processing.Method)) +
        geom_boxplot(alpha = 0.85, width = 0.5, outlier.size = 0.7,
                     outlier.alpha = 0.4, linewidth = 0.4) +
        scale_fill_manual(values = cat_cols(length(unique(d$Processing.Method))),
                          guide = "none") +
        labs(x = NULL, y = measure_label(m)) + theme_coffee() + coord_flip()
    })

    # ── 1. Flavour profile: mean of the 9 attributes per group, overlaid (fmsb) ─
    output$radar_title <- renderText({
      lab <- names(RADAR_GROUPS)[RADAR_GROUPS == input$radar_group]
      sprintf("Flavour profile by %s", tolower(lab))
    })
    output$radar <- renderPlot({
      g <- input$radar_group
      d <- base()
      if (g == "altband") {
        d <- d[!is.na(d$altitude_mean_meters) & d$altitude_mean_meters > 0 &
               d$altitude_mean_meters < 4000, ]
        d$grp <- cut(d$altitude_mean_meters, c(0, 1200, 1600, Inf),
                     c("Low <1200m", "Mid 1200–1600m", "High >1600m"), right = FALSE)
      } else if (g == "moband") {
        d <- d[!is.na(d$Moisture) & d$Moisture > 0, ]
        d$grp <- cut(d$Moisture, c(0, 0.11, 0.12, Inf),
                     c("Dry <11%", "Mid 11–12%", "Damp 12%+"), right = FALSE)
      } else {
        d <- d[d$Processing.Method != "", ]
        d$grp <- d$Processing.Method
      }
      d <- d[!is.na(d$grp), ]
      if (nrow(d) < 3) return(gg_no_data("Not enough data for this slice."))

      # Mean profile per group; keep only groups with at least 5 coffees.
      grps <- split(d, d$grp)
      grps <- grps[vapply(grps, nrow, integer(1)) >= 5]
      if (length(grps) < 1) return(gg_no_data("Groups too small to compare."))
      mat <- t(vapply(grps, function(p) colMeans(p[FLAVOR_ATTRS], na.rm = TRUE),
                      numeric(length(FLAVOR_ATTRS))))
      colnames(mat) <- gsub("\\.", " ", FLAVOR_ATTRS)

      # Absolute = fixed 6–10 on every axis; Relative = zoom each axis to the
      # group range (+20% padding) so small between-group gaps become visible.
      # fmsb wants row 1 = per-axis max, row 2 = per-axis min, then one row/series.
      if ((input$radar_scale %||% "rel") == "abs") {
        radar_df <- as.data.frame(rbind(rep(10, ncol(mat)), rep(6, ncol(mat)), mat))
        axt <- 1; caxis <- c("6", "7", "8", "9", "10")
      } else {
        mins <- apply(mat, 2, min); maxs <- apply(mat, 2, max)
        rng  <- maxs - mins
        pad  <- ifelse(rng < 1e-6, 0.5, rng * 0.20)
        radar_df <- as.data.frame(rbind(maxs + pad, mins - pad, mat))
        axt <- 0; caxis <- NULL
      }
      cols <- cat_cols(nrow(mat))

      op <- par(mar = c(1, 1, 1, 1)); on.exit(par(op))
      radarchart(radar_df, axistype = axt, seg = 4,
                 pcol = cols, pfcol = adjustcolor(cols, alpha.f = 0.18),
                 plwd = 2.5, plty = 1, pty = 16,
                 cglcol = COFFEE_COLS$grid, cglty = 1, cglwd = 0.8,
                 axislabcol = "#B9A88F", caxislabels = caxis, vlcex = 0.85)
      legend("topright", legend = rownames(mat), col = cols, lwd = 2,
             pch = 19, bty = "n", cex = 0.85)
    })

    # ── 2. Quality drivers: overlay chosen attributes vs Total Cup Points ───────
    output$driver_title <- renderText("Sensory attributes vs Total Cup Points")
    output$drivers <- renderPlot({
      attrs <- input$driver_attrs
      if (is.null(attrs) || length(attrs) == 0)
        return(gg_no_data("Tick at least one attribute to compare."))
      d <- base(); d <- d[!is.na(d$Total.Cup.Points), ]
      if (nrow(d) < 3) return(gg_no_data("Not enough data for this slice."))

      # Long format: one labelled series per attribute, label carries its r.
      long <- do.call(rbind, lapply(attrs, function(a) {
        sub <- d[!is.na(d[[a]]), ]
        if (nrow(sub) < 3) return(NULL)
        r <- suppressWarnings(cor(sub[[a]], sub$Total.Cup.Points))
        data.frame(score = sub[[a]], total = sub$Total.Cup.Points,
                   attr = sprintf("%s  (r = %.2f)", gsub("\\.", " ", a), r),
                   stringsAsFactors = FALSE)
      }))
      if (is.null(long) || nrow(long) == 0) return(gg_no_data())

      # Order the legend by r (strongest driver first).
      ord <- order(-as.numeric(sub(".*r = ([0-9.]+).*", "\\1", unique(long$attr))))
      long$attr <- factor(long$attr, levels = unique(long$attr)[ord])
      pal <- cat_cols(nlevels(long$attr))

      p <- ggplot(long, aes(score, total, colour = attr))
      if (length(attrs) <= 3)
        p <- p + geom_point(alpha = 0.3, size = 1.6)
      p +
        geom_smooth(method = "lm", se = FALSE, linewidth = 1.1) +
        scale_colour_manual(values = pal) +
        labs(x = "Attribute score (0–10)", y = "Total Cup Points", colour = NULL) +
        theme_coffee()
    })

    # ── 4. Altitude × moisture: heatmap of the mean measure per band ────────────
    output$am_title <- renderText(
      sprintf("%s by altitude × moisture", measure_label(input$am_measure)))
    output$altmoist <- renderPlot({
      m <- input$am_measure
      d <- base()
      d <- d[!is.na(d$altitude_mean_meters) & d$altitude_mean_meters > 0 &
             d$altitude_mean_meters < 4000 &
             !is.na(d$Moisture) & d$Moisture > 0 & !is.na(d[[m]]), ]
      if (nrow(d) == 0) return(gg_no_data("Not enough data for this slice."))
      d$altband <- cut(d$altitude_mean_meters,
                       c(0, 1000, 1250, 1500, 1750, 2000, Inf),
                       c("<1000", "1000–1250", "1250–1500", "1500–1750",
                         "1750–2000", "2000+"), right = FALSE)
      d$moband  <- cut(d$Moisture, c(0, 0.10, 0.11, 0.12, 0.13, Inf),
                       c("<10%", "10–11%", "11–12%", "12–13%", "13%+"), right = FALSE)
      ag <- aggregate(d[[m]], list(alt = d$altband, mo = d$moband), mean, na.rm = TRUE)
      names(ag) <- c("alt", "mo", "val")

      # Label colour flips with cell brightness so text stays legible on viridis.
      mid <- mean(range(ag$val))
      ggplot(ag, aes(alt, mo, fill = val)) +
        geom_tile(colour = "white") +
        geom_text(aes(label = sprintf("%.1f", val), colour = val > mid),
                  size = 3, show.legend = FALSE) +
        scale_colour_manual(values = c(`TRUE` = "#222222", `FALSE` = "white")) +
        scale_fill_viridis_c(name = measure_label(m)) +
        labs(x = "Altitude band (m)", y = "Moisture") + theme_coffee() +
        theme(axis.text.x = element_text(angle = 25, hjust = 1))
    })
  })
}
