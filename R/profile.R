# Profile tab
# -----------
# Compare up to three countries side by side over a chosen harvest-year range.
# The countries (checkboxes, max 3) and year range are pinned in a left sidebar;
# the body is an accordion of collapsible sections (all expanded on load):
#   • Breakdown        — one treemap per country, side by side (facets)
#   • Score distribution — overlaid histograms; the measure is selectable
#   • Altitude & moisture — two boxplots side by side, grouped by country
#   • Flavour profile  — one radar overlaying every selected country (fmsb)
#
# Reacts to the shared `nav` bus (see server.R): clicking a country on the
# Global tab opens this tab with that country preselected.

library(bslib)
library(fmsb)

# Squarified treemap layout (Bruls, Huizing & van Wijk). Packs `areas` (in the
# given order — sort descending first for the nicest result) into the unit box,
# greedily growing each row while the worst tile aspect ratio keeps improving.
# Returns a data.frame of rectangle coords, one row per area.
squarified_layout <- function(areas) {
  n      <- length(areas)
  scaled <- areas / sum(areas)            # box is 1 x 1, so areas sum to 1
  out    <- data.frame(xmin = numeric(n), xmax = numeric(n),
                       ymin = numeric(n), ymax = numeric(n))

  # Worst (largest) aspect ratio in a row laid along a side of length `side`.
  worst <- function(row, side) {
    s <- sum(row); mx <- max(row); mn <- min(row)
    max((side^2 * mx) / s^2, s^2 / (side^2 * mn))
  }

  fx0 <- 0; fy0 <- 0; fx1 <- 1; fy1 <- 1  # free (unplaced) rectangle
  i <- 1
  while (i <= n) {
    fw <- fx1 - fx0; fh <- fy1 - fy0
    side <- min(fw, fh)                    # lay the row along the shorter side

    row <- scaled[i]; best <- worst(row, side); j <- i + 1
    while (j <= n) {
      cand <- c(row, scaled[j])
      if (worst(cand, side) > best) break
      row <- cand; best <- worst(row, side); j <- j + 1
    }

    thick <- sum(row) / side               # row depth, perpendicular to `side`
    if (fw >= fh) {                         # column on the left, depth along x
      cy <- fy0
      for (k in seq_along(row)) {
        rh <- row[k] / thick
        out[i + k - 1, ] <- c(fx0, fx0 + thick, cy, cy + rh); cy <- cy + rh
      }
      fx0 <- fx0 + thick
    } else {                                # strip on the bottom, depth along y
      cx <- fx0
      for (k in seq_along(row)) {
        rw <- row[k] / thick
        out[i + k - 1, ] <- c(cx, cx + rw, fy0, fy0 + thick); cx <- cx + rw
      }
      fy0 <- fy0 + thick
    }
    i <- j
  }
  out
}

# Build treemap tiles for one country's values (top N categories, rest pooled as
# "Other"). Returns a tidy data.frame (one row per tile) carrying the country
# label and a size-rank used for colour, so several countries can be rbind-ed
# together and drawn as facets. Returns NULL when there's nothing to show.
treemap_tiles <- function(values, n, country_label) {
  values <- values[values != "" & !is.na(values)]
  if (length(values) == 0) return(NULL)
  tab <- as.data.frame(table(values), stringsAsFactors = FALSE)
  names(tab) <- c("cat", "n")
  tab <- tab[order(-tab$n), ]
  if (nrow(tab) > n)
    tab <- rbind(tab[seq_len(n), ],
                 data.frame(cat = "Other", n = sum(tab$n[(n + 1):nrow(tab)])))
  tab$pct <- tab$n / sum(tab$n)
  tab <- cbind(tab, squarified_layout(tab$n))
  tab$cx <- (tab$xmin + tab$xmax) / 2
  tab$cy <- (tab$ymin + tab$ymax) / 2
  tab$lbl <- ifelse(tab$pct >= 0.05, sprintf("%s\n%.0f%%", tab$cat, 100 * tab$pct),
                    as.character(tab$cat))
  tab$isother <- tab$cat == "Other"
  tab$rank <- cumsum(!tab$isother)
  tab$rank[tab$isother] <- 0L              # 0 -> grey "Other"
  tab$country <- country_label
  tab
}

profileUI <- function(id) {
  ns <- NS(id)
  tagList(
    h2("Profile: compare countries"),
    p("Tick up to three countries and a harvest-year range to compare them side ",
      "by side — how each breaks down, how its scores are distributed, its ",
      "growing altitude and moisture, and its flavour profile."),

    layout_sidebar(
      fillable = FALSE,

      # ── Selection pinned on the left ────────────────────────────────────────
      sidebar = sidebar(
        title = "Filters",
        width = 300,
        checkboxGroupInput(ns("countries"), "Countries (up to 3)",
                           choices = character(0)),
        sliderInput(ns("years"), "Harvest year range",
                    min = YEAR_RANGE[1], max = YEAR_RANGE[2],
                    value = YEAR_RANGE, step = 1, sep = "", width = "100%")
      ),

      # ── Collapsible sections (all expanded on load) ─────────────────────────
      accordion(
        open = TRUE,

        # Breakdown: one treemap per country, side by side.
        accordion_panel(
          "Breakdown",
          card(full_screen = TRUE,
               card_header("Breakdown"),
               card_body(
                 plotOutput(ns("tree"), height = "360px"),
                 radioButtons(ns("breakdown"), NULL,
                              choices = c("Processing methods" = "method",
                                          "Regions"             = "region",
                                          "Producers"           = "producer"),
                              selected = "method", inline = TRUE),
                 p(class = "card-note",
                   "One treemap per country. Regions and producers show the top 8; ",
                   "the rest are grouped as “Other”.")))
        ),

        # Over time + score distribution, side by side.
        accordion_panel(
          "Over time & score distribution",
          layout_columns(
            col_widths = c(6, 6),
            card(full_screen = TRUE,
                 card_header(textOutput(ns("timeline_title"))),
                 card_body(
                   plotOutput(ns("timeline"), height = "340px"),
                   selectInput(ns("tl_measure"), "Measure",
                               choices = MEASURE_CHOICES, selected = "Total.Cup.Points"),
                   p(class = "card-note",
                     "Mean of the chosen measure across harvest years, one line per country."))),
            card(full_screen = TRUE,
                 card_header(textOutput(ns("hist_title"))),
                 card_body(
                   plotOutput(ns("hist"), height = "340px"),
                   selectInput(ns("dist_measure"), "Measure",
                               choices = MEASURE_CHOICES, selected = "Total.Cup.Points"),
                   p(class = "card-note",
                     "Overlaid histograms, one colour per country. Pick Total Cup Points or any attribute."))))
        ),

        # Altitude & moisture: two boxplots side by side, grouped by country.
        accordion_panel(
          "Altitude & moisture",
          layout_columns(
            col_widths = c(6, 6),
            card(full_screen = TRUE,
                 card_header("Altitude (m)"),
                 plotOutput(ns("altBox"), height = "340px")),
            card(full_screen = TRUE,
                 card_header("Moisture (%)"),
                 plotOutput(ns("moistBox"), height = "340px"))
          )
        ),

        # Flavour profile radar: one polygon per country, overlaid.
        accordion_panel(
          "Flavour profile",
          card(full_screen = TRUE,
               card_header("Flavour profile"),
               card_body(
                 plotOutput(ns("radar"), height = "420px"),
                 radioButtons(ns("radar_scale"), "Scale",
                              choices = c("Relative" = "rel", "Absolute" = "abs"),
                              selected = "rel", inline = TRUE),
                 p(class = "card-note",
                   "Mean score on each attribute, one polygon per country. Relative ",
                   "zooms each axis to the countries' range to exaggerate differences; ",
                   "Absolute uses a fixed 6–10 scale.")))
        )
      )
    )
  )
}

profileServer <- function(id, data, nav = NULL) {
  moduleServer(id, function(input, output, session) {

    countries <- sort(unique(data$Country.of.Origin[data$Country.of.Origin != ""]))

    # Default to the two most-represented countries so the comparison is visible.
    top2 <- names(sort(table(data$Country.of.Origin[data$Country.of.Origin != ""]),
                       decreasing = TRUE))[1:2]
    updateCheckboxGroupInput(session, "countries",
                             choices = countries, selected = top2)

    # Keep the selection to at most three countries.
    observeEvent(input$countries, {
      if (length(input$countries) > 3)
        updateCheckboxGroupInput(session, "countries",
                                 selected = head(input$countries, 3))
    }, ignoreNULL = FALSE)

    # A country clicked on the Global tab preselects it here (replaces selection).
    if (!is.null(nav)) {
      observeEvent(nav$nonce, {
        req(nav$country, nav$country %in% countries)
        updateCheckboxGroupInput(session, "countries", selected = nav$country)
      }, ignoreInit = TRUE)
    }

    # Selected countries (capped at 3) and a stable colour per country.
    sel <- reactive(head(input$countries[input$countries %in% countries], 3))
    ccols <- reactive({
      cs <- sel()
      setNames(cat_cols(max(1, length(cs))), cs)
    })

    # Rows for the selected countries within the year range.
    flt <- reactive({
      d <- data[data$Country.of.Origin %in% sel(), ]
      d[!is.na(d$harvest_year) &
        d$harvest_year >= input$years[1] & d$harvest_year <= input$years[2], ]
    })

    # ── Breakdown: one treemap per country (facets) ─────────────────────────────
    output$tree <- renderPlot({
      cs <- sel()
      if (length(cs) == 0) return(gg_no_data("Select 1–3 countries to compare."))
      d   <- flt()
      col <- switch(input$breakdown, method = "Processing.Method",
                    region = "Region", producer = "Producer", "Processing.Method")
      ntop <- if (input$breakdown == "method") 100 else 8

      tiles <- do.call(rbind, lapply(cs, function(c)
        treemap_tiles(d[[col]][d$Country.of.Origin == c], ntop, c)))
      if (is.null(tiles) || nrow(tiles) == 0)
        return(gg_no_data("No data for this selection."))

      tiles$country <- factor(tiles$country, levels = cs)
      ranks <- max(1, max(tiles$rank))
      pal   <- c("0" = "#9ca0b0",
                 setNames(cat_cols(ranks), as.character(seq_len(ranks))))

      ggplot(tiles) +
        geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
                      fill = factor(rank)), colour = "white", linewidth = 0.8) +
        geom_text(aes(x = cx, y = cy, label = lbl), size = 2.6, colour = "white",
                  lineheight = 0.9) +
        scale_fill_manual(values = pal) +
        facet_wrap(~ country, nrow = 1) +
        coord_equal(expand = FALSE) +
        theme_void(base_size = 12) +
        theme(legend.position = "none",
              strip.text = element_text(face = "bold", colour = LATTE$text, size = 12))
    })

    # ── Score distribution: overlaid histograms of the chosen measure ───────────
    output$hist_title <- renderText(
      sprintf("%s distribution", measure_label(input$dist_measure)))
    output$hist <- renderPlot({
      if (length(sel()) == 0) return(gg_no_data("Select 1–3 countries to compare."))
      m <- input$dist_measure
      d <- flt(); d <- d[!is.na(d[[m]]), ]
      if (m == "Total.Cup.Points") d <- d[d[[m]] > 0, ]
      if (nrow(d) == 0) return(gg_no_data("No data for this selection."))
      bw <- if (m == "Total.Cup.Points") 1 else 0.25
      ggplot(d, aes(.data[[m]], fill = Country.of.Origin)) +
        geom_histogram(position = "identity", alpha = 0.45, binwidth = bw,
                       colour = "white") +
        scale_fill_manual(values = ccols(), name = NULL) +
        labs(x = measure_label(m), y = "Coffees") + theme_coffee()
    })

    # ── Altitude boxplot, grouped by country ────────────────────────────────────
    output$altBox <- renderPlot({
      if (length(sel()) == 0) return(gg_no_data("Select countries."))
      d <- flt()
      d <- d[!is.na(d$altitude_mean_meters) & d$altitude_mean_meters > 0 &
             d$altitude_mean_meters < 4000, ]
      if (nrow(d) == 0) return(gg_no_data("No altitude data for this selection."))
      ggplot(d, aes(Country.of.Origin, altitude_mean_meters,
                    fill = Country.of.Origin)) +
        geom_boxplot(alpha = 0.85, width = 0.5, outlier.size = 0.7,
                     outlier.alpha = 0.4, linewidth = 0.4) +
        scale_fill_manual(values = ccols(), guide = "none") +
        labs(x = NULL, y = "Altitude (m)") + theme_coffee() +
        theme(axis.text.x = element_text(angle = 20, hjust = 1))
    })

    # ── Moisture boxplot, grouped by country ────────────────────────────────────
    output$moistBox <- renderPlot({
      if (length(sel()) == 0) return(gg_no_data("Select countries."))
      d <- flt(); d <- d[!is.na(d$Moisture) & d$Moisture > 0, ]
      if (nrow(d) == 0) return(gg_no_data("No moisture data for this selection."))
      ggplot(d, aes(Country.of.Origin, Moisture * 100, fill = Country.of.Origin)) +
        geom_boxplot(alpha = 0.85, width = 0.5, outlier.size = 0.7,
                     outlier.alpha = 0.4, linewidth = 0.4) +
        scale_fill_manual(values = ccols(), guide = "none") +
        labs(x = NULL, y = "Moisture (%)") + theme_coffee() +
        theme(axis.text.x = element_text(angle = 20, hjust = 1))
    })

    # ── Over time: mean measure per harvest year, one line per country ───────────
    output$timeline_title <- renderText(
      sprintf("%s over time", measure_label(input$tl_measure)))
    output$timeline <- renderPlot({
      if (length(sel()) == 0) return(gg_no_data("Select 1–3 countries to compare."))
      m <- input$tl_measure
      d <- flt(); d <- d[!is.na(d[[m]]) & !is.na(d$harvest_year), ]
      if (m == "Total.Cup.Points") d <- d[d[[m]] > 0, ]
      if (nrow(d) == 0) return(gg_no_data("No data for this selection."))
      ag <- aggregate(d[[m]], list(year = d$harvest_year,
                                   country = d$Country.of.Origin), mean, na.rm = TRUE)
      names(ag) <- c("year", "country", "val")
      ggplot(ag, aes(year, val, colour = country)) +
        geom_line(linewidth = 1) + geom_point(size = 2.4) +
        scale_colour_manual(values = ccols(), name = NULL) +
        scale_x_continuous(breaks = sort(unique(ag$year))) +
        labs(x = "Harvest year", y = paste("Mean", measure_label(m))) + theme_coffee()
    })

    # ── Flavour radar: one polygon per country, overlaid (fmsb) ──────────────────
    output$radar <- renderPlot({
      cs <- sel()
      if (length(cs) == 0) {
        plot.new(); text(0.5, 0.5, "Select 1–3 countries to compare.",
                         col = "#888888", cex = 1.1); return()
      }
      d <- flt()
      mat <- t(vapply(cs, function(c) {
        p <- d[d$Country.of.Origin == c &
               !is.na(d$Total.Cup.Points) & d$Total.Cup.Points > 0, ]
        if (nrow(p) == 0) return(rep(NA_real_, length(FLAVOR_ATTRS)))
        colMeans(p[FLAVOR_ATTRS], na.rm = TRUE)
      }, numeric(length(FLAVOR_ATTRS))))
      colnames(mat) <- gsub("\\.", " ", FLAVOR_ATTRS)
      rownames(mat) <- cs
      mat <- mat[rowSums(is.na(mat)) == 0, , drop = FALSE]   # drop empty countries
      if (nrow(mat) < 1) {
        plot.new(); text(0.5, 0.5, "No data for this selection.",
                         col = "#888888", cex = 1.1); return()
      }

      # Absolute = fixed 6–10 on every axis; Relative = zoom each axis to the
      # countries' range (+20% padding) to surface otherwise-tiny gaps.
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
      cols <- ccols()[rownames(mat)]

      op <- par(mar = c(1, 1, 1, 1)); on.exit(par(op))
      radarchart(radar_df, axistype = axt, seg = 4,
                 pcol = cols, pfcol = adjustcolor(cols, alpha.f = 0.15),
                 plwd = 2.5, plty = 1, pty = 16,
                 cglcol = COFFEE_COLS$grid, cglty = 1, cglwd = 0.8,
                 axislabcol = "#B9A88F", caxislabels = caxis, vlcex = 0.85)
      legend("topright", legend = rownames(mat), col = cols, lwd = 2,
             pch = 19, bty = "n", cex = 0.85)
    })
  })
}
