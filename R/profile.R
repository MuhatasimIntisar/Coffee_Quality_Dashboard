# Profile tab  — single-country deep dive
# ---------------------------------------
# The "small view" in the dashboard's overview→detail flow: pick ONE country and
# see its full fingerprint. The country + year range are pinned in a left
# sidebar; the body is an accordion (all expanded on load):
#   • Breakdown           — a treemap of methods / regions / producers
#   • Over time & scores  — a trend line and a score histogram, side by side
#   • Altitude & moisture — two boxplots side by side
#   • Flavour profile     — a radar of the country vs the all-coffee average
#
# Reacts to the shared `nav` bus (see server.R): clicking a country on the
# Global tab opens this tab with that country loaded (details-on-demand).

library(bslib)
library(fmsb)
library(plotly)

# Squarified treemap layout (Bruls, Huizing & van Wijk). Packs `areas` (sorted
# descending first) into the unit box, growing each row while the worst tile
# aspect ratio keeps improving. Returns rectangle coords, one row per area.
squarified_layout <- function(areas) {
  n      <- length(areas)
  scaled <- areas / sum(areas)
  out    <- data.frame(xmin = numeric(n), xmax = numeric(n),
                       ymin = numeric(n), ymax = numeric(n))
  worst <- function(row, side) {
    s <- sum(row); mx <- max(row); mn <- min(row)
    max((side^2 * mx) / s^2, s^2 / (side^2 * mn))
  }
  fx0 <- 0; fy0 <- 0; fx1 <- 1; fy1 <- 1
  i <- 1
  while (i <= n) {
    fw <- fx1 - fx0; fh <- fy1 - fy0
    side <- min(fw, fh)
    row <- scaled[i]; best <- worst(row, side); j <- i + 1
    while (j <= n) {
      cand <- c(row, scaled[j])
      if (worst(cand, side) > best) break
      row <- cand; best <- worst(row, side); j <- j + 1
    }
    thick <- sum(row) / side
    if (fw >= fh) {
      cy <- fy0
      for (k in seq_along(row)) {
        rh <- row[k] / thick
        out[i + k - 1, ] <- c(fx0, fx0 + thick, cy, cy + rh); cy <- cy + rh
      }
      fx0 <- fx0 + thick
    } else {
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

# Treemap of a categorical column: top N categories by count, the rest pooled
# into "Other". Tile area is the category's share; labelled in place.
count_treemap <- function(values, n) {
  values <- values[values != "" & !is.na(values)]
  if (length(values) == 0) return(gg_no_data("No data for this selection."))
  tab <- as.data.frame(table(values), stringsAsFactors = FALSE)
  names(tab) <- c("cat", "n")
  tab <- tab[order(-tab$n), ]
  if (nrow(tab) > n)
    tab <- rbind(tab[seq_len(n), ],
                 data.frame(cat = "Other", n = sum(tab$n[(n + 1):nrow(tab)])))
  tab$cat <- factor(tab$cat, levels = tab$cat)
  tab$pct <- tab$n / sum(tab$n)

  k         <- nlevels(tab$cat)
  has_other <- "Other" %in% tab$cat
  base      <- cat_cols(max(1, k - has_other))
  pal       <- if (has_other) c(base, "#9ca0b0") else base

  tab    <- cbind(tab, squarified_layout(tab$n))
  tab$cx <- (tab$xmin + tab$xmax) / 2
  tab$cy <- (tab$ymin + tab$ymax) / 2
  tab$lbl <- ifelse(tab$pct >= 0.04,
                    sprintf("%s\n%.0f%%", tab$cat, 100 * tab$pct),
                    as.character(tab$cat))

  ggplot(tab) +
    geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = cat),
              colour = "white", linewidth = 1) +
    geom_text(aes(x = cx, y = cy, label = lbl), size = 3.1, colour = "white",
              lineheight = 0.9) +
    scale_fill_manual(values = setNames(pal, levels(tab$cat)), name = NULL) +
    coord_equal(expand = FALSE) +
    theme_void(base_size = 12) +
    theme(legend.position = "none")
}

profileUI <- function(id) {
  ns <- NS(id)
  tagList(
    h2("Profile"),
    h5("Analyze a single origin."),
    p("Select a country to examine its characteristics in detail: the ",
      "composition of its coffees, the distribution of its scores, its growing ",
      "altitude and moisture, its flavour profile, and its development over time."),

    # Selection at the top (next to the title) rather than a side filter.
    wellPanel(
      fluidRow(
        column(5,
          selectInput(ns("country"), "Country", choices = character(0))),
        column(7,
          sliderInput(ns("years"), "Harvest year range",
                      min = YEAR_RANGE[1], max = YEAR_RANGE[2],
                      value = YEAR_RANGE, step = 1, sep = "", width = "100%"))
      )
    ),

    # This country's average scorecard (ring + legend), mirroring the Overview page.
    scorecard_card(plotOutput(ns("country_ring"), height = "300px"),
                   uiOutput(ns("country_legend")),
                   n = textOutput(ns("sc_count"), inline = TRUE)),

      accordion(
        open = "Breakdown",

        # Breakdown: treemap (share) + a bar chart (ranked by score), side by side.
        accordion_panel(
          "Breakdown",
          radioButtons(ns("breakdown"), NULL,
                       choices = c("Regions"   = "region",
                                   "Producers" = "producer"),
                       selected = "region", inline = TRUE),
          layout_columns(
            col_widths = c(6, 6),
            card(full_screen = TRUE,
                 card_header("Share of the country's coffees"),
                 card_body(
                   p(class = "card-note",
                     "Tile size represents each category's share of coffees. The ",
                     "eight largest regions or producers are shown; the remainder ",
                     "are grouped as “Other”."),
                   div(style = "position:relative;",
                       plotOutput(ns("tree"), height = "360px",
                                  hover = hoverOpts(ns("tree_hover"), delay = 30,
                                                    delayType = "throttle")),
                       uiOutput(ns("tree_tooltip"))))),
            card(full_screen = TRUE,
                 card_header(textOutput(ns("bd_bar_title"))),
                 card_body(
                   p(class = "card-note",
                     "Each point represents one category; hover to display its name. ",
                     "Average score is plotted against the number of coffees, so ",
                     "points further right are supported by more observations."),
                   div(style = "position:relative;",
                       plotOutput(ns("bd_bar"), height = "360px",
                                  hover = hoverOpts(ns("bd_hover"), delay = 30,
                                                    delayType = "throttle")),
                       uiOutput(ns("bd_tooltip"))))))
        ),

        # Method: a score-vs-count scatter + a pie of the method mix (method only).
        accordion_panel(
          "Method",
          layout_columns(
            col_widths = c(6, 6),
            card(full_screen = TRUE,
                 card_header("Share of coffees by method"),
                 card_body(
                   p(class = "card-note",
                     "The distribution of this country's coffees across processing ",
                     "methods."),
                   plotlyOutput(ns("method_pie"), height = "340px"))),
            card(full_screen = TRUE,
                 card_header("Cup score by method"),
                 card_body(
                   p(class = "card-note",
                     "The distribution of cup scores for each processing method ",
                     "(methods with at least three coffees)."),
                   plotOutput(ns("method_box"), height = "340px"))))
        ),

        # Score distribution.
        accordion_panel(
          "Score distribution",
          card(full_screen = TRUE,
               card_header(textOutput(ns("hist_title"))),
               card_body(
                 selectInput(ns("dist_measure"), "Measure",
                             choices = MEASURE_CHOICES, selected = "Total.Cup.Points"),
                 plotOutput(ns("hist"), height = "340px"),
                 uiOutput(ns("hist_theory"))))
        ),

        # Altitude & moisture: two boxplots side by side.
        accordion_panel(
          "Altitude & moisture",
          p(class = "card-note",
            "The dashed line on each plot indicates the average across all ",
            "coffees, for comparison."),
          layout_columns(
            col_widths = c(6, 6),
            card(full_screen = TRUE,
                 card_header("Altitude (m)"),
                 plotOutput(ns("altBox"), height = "340px")),
            card(full_screen = TRUE,
                 card_header("Moisture (%)"),
                 plotOutput(ns("moistBox"), height = "340px"))
          ),
          uiOutput(ns("altmoist_theory"))
        ),

        # Flavour profile radar: the country against the all-coffee average.
        accordion_panel(
          "Flavour profile",
          card(full_screen = TRUE,
               card_header("Flavour profile"),
               card_body(
                 radioButtons(ns("radar_scale"), "Scale",
                              choices = c("Relative" = "rel", "Absolute" = "abs"),
                              selected = "rel", inline = TRUE),
                 p(class = "card-note",
                   "The country's mean for each attribute (filled) is shown against ",
                   "the average across all coffees (dashed). The Relative option ",
                   "scales each axis to the observed range; the Absolute option ",
                   "uses a fixed scale from 6 to 10."),
                 plotOutput(ns("radar"), height = "420px")))
        )
      )
  )
}

profileServer <- function(id, data, nav = NULL) {
  moduleServer(id, function(input, output, session) {

    countries <- sort(unique(data$Country.of.Origin[data$Country.of.Origin != ""]))
    default_country <- names(sort(
      table(data$Country.of.Origin[data$Country.of.Origin != ""]),
      decreasing = TRUE))[1]
    updateSelectInput(session, "country", choices = countries,
                      selected = default_country)

    # A country clicked on the Global tab loads it here (details-on-demand).
    if (!is.null(nav)) {
      observeEvent(nav$nonce, {
        req(nav$country, nav$country %in% countries)
        updateSelectInput(session, "country", selected = nav$country)
      }, ignoreInit = TRUE)
    }

    # ── Selected slice (one country + year range) ───────────────────────────────
    flt <- reactive({
      d <- data
      if (!is.null(input$country) && input$country != "")
        d <- d[d$Country.of.Origin == input$country, ]
      d[!is.na(d$harvest_year) &
        d$harvest_year >= input$years[1] & d$harvest_year <= input$years[2], ]
    })
    scored_slice <- reactive({
      s <- flt(); s[!is.na(s$Total.Cup.Points) & s$Total.Cup.Points > 0, ]
    })

    # ── Breakdown treemap ───────────────────────────────────────────────────────
    output$tree <- renderPlot({
      d <- flt()
      col <- switch(input$breakdown, method = "Processing.Method",
                    region = "Region", producer = "Producer", "Processing.Method")
      ntop <- if (input$breakdown == "method") 100 else 8
      count_treemap(d[[col]], ntop)
    })

    # Treemap tile geometry, recomputed identically to count_treemap() so the
    # hover tooltip can identify which tile the cursor is over.
    tree_layout <- reactive({
      d <- flt()
      col <- switch(input$breakdown, method = "Processing.Method",
                    region = "Region", producer = "Producer", "Processing.Method")
      ntop <- if (input$breakdown == "method") 100 else 8
      keep   <- d[[col]] != "" & !is.na(d[[col]])
      values <- d[[col]][keep]; bagv <- d$Number.of.Bags[keep]
      if (length(values) == 0) return(NULL)
      tab <- as.data.frame(table(values), stringsAsFactors = FALSE)
      names(tab) <- c("cat", "n"); tab <- tab[order(-tab$n), ]
      tab$bags <- as.numeric(tapply(bagv, values, function(x) sum(x, na.rm = TRUE))[tab$cat])
      if (nrow(tab) > ntop)
        tab <- rbind(tab[seq_len(ntop), ],
                     data.frame(cat = "Other",
                                n    = sum(tab$n[(ntop + 1):nrow(tab)]),
                                bags = sum(tab$bags[(ntop + 1):nrow(tab)])))
      cbind(tab, squarified_layout(tab$n))
    })
    output$tree_tooltip <- renderUI({
      tab <- tree_layout(); hv <- input$tree_hover
      if (is.null(tab) || is.null(hv)) return(NULL)
      hit <- tab[hv$x >= tab$xmin & hv$x <= tab$xmax &
                 hv$y >= tab$ymin & hv$y <= tab$ymax, ]
      if (nrow(hit) == 0) return(NULL)
      hit <- hit[1, ]
      div(style = paste0("position:absolute; z-index:100; pointer-events:none;",
                         " left:", hv$coords_css$x + 10, "px; top:",
                         hv$coords_css$y + 10, "px;",
                         " background:#FFFFFF; border:1px solid #EADDCB;",
                         " border-radius:8px; padding:5px 9px; font-size:13px;",
                         " color:#2B2018; box-shadow:0 1px 4px rgba(58,36,23,.12);"),
          tags$b(as.character(hit$cat)), tags$br(),
          sprintf("%d coffees · %s bags", hit$n,
                  format(hit$bags, big.mark = ",", trim = TRUE)))
    })

    # ── Breakdown scatter: average score (y) vs number of coffees (x) ───────────
    # Per-category stats, shared by the plot and the hover tooltip.
    bd_stats <- reactive({
      col <- switch(input$breakdown, method = "Processing.Method",
                    region = "Region", producer = "Producer", "Processing.Method")
      d <- scored_slice()
      d <- d[d[[col]] != "" & !is.na(d[[col]]) & !is.na(d$Total.Cup.Points), ]
      if (nrow(d) == 0) return(NULL)
      parts <- split(d$Total.Cup.Points, d[[col]])
      data.frame(cat = names(parts), score = sapply(parts, mean),
                 n = sapply(parts, length), stringsAsFactors = FALSE)
    })
    output$bd_bar_title <- renderText({
      lab <- switch(input$breakdown, method = "processing method",
                    region = "region", producer = "producer", "category")
      sprintf("Score vs number of coffees by %s", lab)
    })
    output$bd_bar <- renderPlot({
      stats <- bd_stats()
      if (is.null(stats)) return(gg_no_data("No data for this selection."))
      ggplot(stats, aes(n, score)) +
        geom_point(colour = COFFEE_COLS$blue, alpha = 0.75, size = 3) +
        labs(x = "Number of coffees", y = "Average cup score") +
        coord_cartesian(ylim = measure_limits("Total.Cup.Points")) + theme_coffee()
    })
    # Floating tooltip showing the category name nearest the cursor.
    output$bd_tooltip <- renderUI({
      stats <- bd_stats(); hv <- input$bd_hover
      if (is.null(stats) || is.null(hv)) return(NULL)
      pt <- nearPoints(stats, hv, xvar = "n", yvar = "score",
                       threshold = 18, maxpoints = 1)
      if (nrow(pt) == 0) return(NULL)
      div(style = paste0("position:absolute; z-index:100; pointer-events:none;",
                         " left:", hv$coords_css$x + 10, "px; top:",
                         hv$coords_css$y + 10, "px;",
                         " background:#FFFFFF; border:1px solid #EADDCB;",
                         " border-radius:8px; padding:5px 9px; font-size:13px;",
                         " color:#2B2018; box-shadow:0 1px 4px rgba(58,36,23,.12);"),
          tags$b(pt$cat), tags$br(),
          sprintf("Average score %.1f · %d coffees", pt$score, pt$n))
    })

    # ── Method panel: method-mix pie + cup-score boxplot per method ─────────────
    # Cup-score distribution per processing method (methods with >= 3 coffees).
    output$method_box <- renderPlot({
      d <- scored_slice()
      d <- d[d$Processing.Method != "" & !is.na(d$Processing.Method), ]
      keep <- names(which(table(d$Processing.Method) >= 3))
      d <- d[d$Processing.Method %in% keep, ]
      if (nrow(d) == 0) return(gg_no_data("No data for this selection."))
      ggplot(d, aes(reorder(Processing.Method, Total.Cup.Points, FUN = median),
                    Total.Cup.Points, fill = Processing.Method)) +
        geom_boxplot(alpha = 0.85, width = 0.5, outlier.size = 0.7,
                     outlier.alpha = 0.4, linewidth = 0.4) +
        scale_fill_manual(values = cat_cols(length(unique(d$Processing.Method))),
                          guide = "none") +
        labs(x = "Processing method", y = "Total Cup Points") +
        theme_coffee() + coord_flip(ylim = measure_limits("Total.Cup.Points"))
    })
    output$method_pie <- renderPlotly({
      d <- flt()
      d <- d[d$Processing.Method != "" & !is.na(d$Processing.Method), ]
      validate(need(nrow(d) > 0, "No data for this selection."))
      tab <- as.data.frame(table(method = d$Processing.Method), stringsAsFactors = FALSE)
      names(tab) <- c("method", "n")
      tab$bags <- as.numeric(tapply(d$Number.of.Bags, d$Processing.Method,
                                    function(x) sum(x, na.rm = TRUE))[tab$method])
      tab <- tab[order(-tab$n), ]
      plot_ly(tab, labels = ~method, values = ~n, type = "pie", sort = FALSE,
              customdata = ~bags,
              textinfo = "percent", textfont = list(color = "#FFFFFF"),
              marker = list(colors = cat_cols(nrow(tab)),
                            line = list(color = "#FFFFFF", width = 1)),
              hovertemplate = paste0("<b>%{label}</b><br>%{value} coffees",
                                     " · %{customdata:,.0f} bags<extra></extra>")) |>
        layout(showlegend = TRUE, legend = list(font = list(size = 12)),
               paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
               margin = list(l = 10, r = 10, t = 10, b = 10)) |>
        config(displayModeBar = FALSE)
    })

    # ── Country scorecard: ring + progress-bar legend (shared helpers) ──────────
    # Dataset-wide component averages, drawn as a comparison line on each bar.
    avg_ref <- scorecard_df(
      data[!is.na(data$Total.Cup.Points) & data$Total.Cup.Points > 0, ])$val

    output$country_ring <- renderPlot({
      s <- scored_slice()
      if (nrow(s) == 0) return(gg_no_data("No data for this selection."))
      scorecard_ring(scorecard_df(s))
    })
    output$country_legend <- renderUI({
      s <- scored_slice()
      if (nrow(s) == 0) return(NULL)
      scorecard_legend(scorecard_df(s), ref = avg_ref)
    })
    output$sc_count <- renderText(
      sprintf("%s coffees evaluated", format(nrow(scored_slice()), big.mark = ",")))

    # ── Score distribution: histogram of the chosen measure ─────────────────────
    output$hist_title <- renderText(
      sprintf("%s distribution", measure_label(input$dist_measure)))
    output$hist <- renderPlot({
      m <- input$dist_measure
      d <- scored_slice(); d <- d[!is.na(d[[m]]), ]
      if (m == "Total.Cup.Points") d <- d[d[[m]] > 0, ]
      if (nrow(d) == 0) return(gg_no_data("No data for this selection."))
      bw <- if (m == "Total.Cup.Points") 1 else 0.25
      all_d <- data[!is.na(data$Total.Cup.Points) & data$Total.Cup.Points > 0, ]
      ggplot(d, aes(.data[[m]])) +
        geom_histogram(binwidth = bw, fill = COFFEE_COLS$blue, colour = "white") +
        geom_vline(xintercept = mean(all_d[[m]], na.rm = TRUE), linetype = "dashed",
                   colour = COFFEE_COLS$purple, linewidth = 0.8) +
        labs(x = measure_label(m), y = "Coffees") +
        coord_cartesian(xlim = measure_limits(m)) + theme_coffee()
    })

    # Dynamic reading note: compares this country's average with the overall
    # average for the selected measure, from the same data as the histogram.
    output$hist_theory <- renderUI({
      m <- input$dist_measure
      d <- scored_slice(); d <- d[!is.na(d[[m]]), ]
      if (m == "Total.Cup.Points") d <- d[d[[m]] > 0, ]
      if (nrow(d) == 0) return(NULL)
      all_d <- data[!is.na(data$Total.Cup.Points) & data$Total.Cup.Points > 0, ]
      all_m <- mean(all_d[[m]], na.rm = TRUE)
      cty   <- if (is.null(input$country) || input$country == "") "This country"
               else input$country
      gap   <- mean(d[[m]]) - all_m
      gap_txt <- if (abs(gap) < 0.05) "closely in line with the overall average"
      else sprintf("about %.1f points %s the overall average", abs(gap),
                   if (gap > 0) "above" else "below")
      p(class = "card-note", sprintf(
        paste0("The bars show how %s's coffees are distributed on %s; the dashed ",
               "line marks the average across all coffees (%.1f). %s's average of ",
               "%.1f is %s, based on %d graded coffees."),
        cty, measure_label(m), all_m, cty, mean(d[[m]]), gap_txt, nrow(d)))
    })

    # ── Altitude boxplot ────────────────────────────────────────────────────────
    output$altBox <- renderPlot({
      d <- flt()
      d <- d[!is.na(d$altitude_mean_meters) & d$altitude_mean_meters > 0 &
             d$altitude_mean_meters < 4000, ]
      if (nrow(d) == 0) return(gg_no_data("No altitude data for this selection."))
      all_alt <- data$altitude_mean_meters[!is.na(data$altitude_mean_meters) &
                   data$altitude_mean_meters > 0 & data$altitude_mean_meters < 4000]
      ggplot(d, aes(x = "", y = altitude_mean_meters)) +
        geom_boxplot(fill = COFFEE_COLS$green, alpha = 0.85, width = 0.4,
                     outlier.size = 0.7, outlier.alpha = 0.4, linewidth = 0.4) +
        geom_hline(yintercept = mean(all_alt), linetype = "dashed",
                   colour = COFFEE_COLS$purple, linewidth = 0.8) +
        labs(x = NULL, y = "Altitude (m)") +
        coord_cartesian(ylim = ALT_LIMITS) + theme_coffee()
    })

    # ── Moisture boxplot ────────────────────────────────────────────────────────
    output$moistBox <- renderPlot({
      d <- flt(); d <- d[!is.na(d$Moisture) & d$Moisture > 0, ]
      if (nrow(d) == 0) return(gg_no_data("No moisture data for this selection."))
      all_moist <- data$Moisture[!is.na(data$Moisture) & data$Moisture > 0] * 100
      ggplot(d, aes(x = "", y = Moisture * 100)) +
        geom_boxplot(fill = COFFEE_COLS$green, alpha = 0.85, width = 0.4,
                     outlier.size = 0.7, outlier.alpha = 0.4, linewidth = 0.4) +
        geom_hline(yintercept = mean(all_moist), linetype = "dashed",
                   colour = COFFEE_COLS$purple, linewidth = 0.8) +
        labs(x = NULL, y = "Moisture (%)") +
        coord_cartesian(ylim = MOIST_LIMITS) + theme_coffee()
    })

    # Dynamic note: this country's growing conditions against the dataset average.
    output$altmoist_theory <- renderUI({
      d <- flt()
      alt <- d$altitude_mean_meters[!is.na(d$altitude_mean_meters) &
               d$altitude_mean_meters > 0 & d$altitude_mean_meters < 4000]
      moi <- d$Moisture[!is.na(d$Moisture) & d$Moisture > 0] * 100
      if (length(alt) == 0 || length(moi) == 0) return(NULL)
      all_alt <- data$altitude_mean_meters[!is.na(data$altitude_mean_meters) &
                   data$altitude_mean_meters > 0 & data$altitude_mean_meters < 4000]
      p(class = "card-note", sprintf(
        paste0("This country's coffees are grown at an average altitude of %s m, ",
               "against a dataset average of %s m, with mean moisture of %.1f%%."),
        format(round(mean(alt)), big.mark = ","),
        format(round(mean(all_alt)), big.mark = ","), mean(moi)))
    })

    # ── Flavour radar: the country vs the all-coffee average (context only) ─────
    output$radar <- renderPlot({
      s <- scored_slice()
      if (nrow(s) == 0) {
        plot.new(); text(0.5, 0.5, "No data for this selection.",
                         col = "#888888", cex = 1.1); return()
      }
      cn  <- if (is.null(input$country) || input$country == "") "Country" else input$country
      all_scored <- data[!is.na(data$Total.Cup.Points) & data$Total.Cup.Points > 0, ]
      mat <- rbind(colMeans(s[FLAVOR_ATTRS], na.rm = TRUE),
                   colMeans(all_scored[FLAVOR_ATTRS], na.rm = TRUE))
      rownames(mat) <- c(cn, "All coffees (avg)")
      colnames(mat) <- gsub("\\.", " ", FLAVOR_ATTRS)

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
      cols <- c(COFFEE_COLS$green, COFFEE_COLS$grey)

      # This country drawn solid and filled; the all-coffee average drawn as a
      # dashed outline (no fill) so the two series read apart at a glance.
      op <- par(mar = c(1, 1, 1, 1)); on.exit(par(op))
      radarchart(radar_df, axistype = axt, seg = 4,
                 pcol = cols, pfcol = c(adjustcolor(cols[1], alpha.f = 0.18), NA),
                 plwd = c(2.5, 2), plty = c(1, 2), pty = 16,
                 cglcol = COFFEE_COLS$grid, cglty = 1, cglwd = 0.8,
                 axislabcol = "#B9A88F", caxislabels = caxis, vlcex = 0.85)
      legend("topright", legend = rownames(mat), col = cols, lwd = 2,
             lty = c(1, 2), bty = "n", cex = 0.85)
    })
  })
}
