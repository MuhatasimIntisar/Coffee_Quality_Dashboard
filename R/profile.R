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
    h2("Profile: country deep dive"),
    p("Pick one country to see its full fingerprint — how its coffee breaks ",
      "down, how its scores are distributed, its growing altitude and moisture, ",
      "its flavour profile, and how it has changed over time."),

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
                   uiOutput(ns("country_legend"))),

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
                   plotOutput(ns("tree"), height = "360px"),
                   p(class = "card-note",
                     "Tile size = share of coffees. Regions and producers show the ",
                     "top 8; the rest grouped as “Other”."))),
            card(full_screen = TRUE,
                 card_header(textOutput(ns("bd_bar_title"))),
                 card_body(
                   div(style = "position:relative;",
                       plotOutput(ns("bd_bar"), height = "360px",
                                  hover = hoverOpts(ns("bd_hover"), delay = 30,
                                                    delayType = "throttle")),
                       uiOutput(ns("bd_tooltip"))),
                   p(class = "card-note",
                     "Each point is a category — hover to see its name. Average score ",
                     "vs how many coffees it has; points further right rest on more data."))))
        ),

        # Method: a score-vs-count scatter + a pie of the method mix (method only).
        accordion_panel(
          "Method",
          layout_columns(
            col_widths = c(6, 6),
            card(full_screen = TRUE,
                 card_header("Share of coffees by method"),
                 card_body(
                   plotOutput(ns("method_pie"), height = "340px"),
                   p(class = "card-note",
                     "How this country's coffees split across processing methods."))),
            card(full_screen = TRUE,
                 card_header("Cup score by method"),
                 card_body(
                   plotOutput(ns("method_box"), height = "340px"),
                   p(class = "card-note",
                     "Cup-score spread for each method (methods with at least 3 ",
                     "coffees)."))))
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
                 p(class = "card-note",
                   "How the chosen measure is spread across this country's coffees. ",
                   "The dashed line is the all-coffee average.")))
        ),

        # Altitude & moisture: two boxplots side by side.
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
          ),
          p(class = "card-note",
            "The dashed line on each is the all-coffee average, for comparison.")
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
                 plotOutput(ns("radar"), height = "420px"),
                 p(class = "card-note",
                   "This country's mean on each attribute (filled) against the ",
                   "all-coffee average (dashed) for context. Relative zooms each ",
                   "axis to the range; Absolute uses a fixed 6–10 scale.")))
        )
      ),

    hr(),
    p(style = "font-size:17px; color:#2B2018; max-width:860px;",
      "That's one origin up close. Now see what growing & processing choices drive ",
      "quality across ", em("all"), " coffee — the ", strong("Attributing Factors"),
      " tab.")
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
        labs(x = "Number of coffees", y = "Average cup score") + theme_coffee()
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
          sprintf("Avg score %.1f · %d coffees", pt$score, pt$n))
    })

    # ── Method panel: score-vs-count scatter + a pie of the method mix ──────────
    m_stats <- reactive({
      d <- scored_slice()
      d <- d[d$Processing.Method != "" & !is.na(d$Processing.Method) &
             !is.na(d$Total.Cup.Points), ]
      if (nrow(d) == 0) return(NULL)
      parts <- split(d$Total.Cup.Points, d$Processing.Method)
      data.frame(cat = names(parts), score = sapply(parts, mean),
                 n = sapply(parts, length), stringsAsFactors = FALSE)
    })
    output$method_scatter <- renderPlot({
      s <- m_stats()
      if (is.null(s)) return(gg_no_data("No data for this selection."))
      ggplot(s, aes(n, score)) +
        geom_point(colour = COFFEE_COLS$blue, alpha = 0.75, size = 3) +
        labs(x = "Number of coffees", y = "Average cup score") + theme_coffee()
    })
    output$ms_tooltip <- renderUI({
      s <- m_stats(); hv <- input$ms_hover
      if (is.null(s) || is.null(hv)) return(NULL)
      pt <- nearPoints(s, hv, xvar = "n", yvar = "score",
                       threshold = 18, maxpoints = 1)
      if (nrow(pt) == 0) return(NULL)
      div(style = paste0("position:absolute; z-index:100; pointer-events:none;",
                         " left:", hv$coords_css$x + 10, "px; top:",
                         hv$coords_css$y + 10, "px;",
                         " background:#FFFFFF; border:1px solid #EADDCB;",
                         " border-radius:8px; padding:5px 9px; font-size:13px;",
                         " color:#2B2018; box-shadow:0 1px 4px rgba(58,36,23,.12);"),
          tags$b(pt$cat), tags$br(),
          sprintf("Avg score %.1f · %d coffees", pt$score, pt$n))
    })
    output$method_pie <- renderPlot({
      d <- flt()
      d <- d[d$Processing.Method != "" & !is.na(d$Processing.Method), ]
      if (nrow(d) == 0) return(gg_no_data("No data for this selection."))
      tab <- as.data.frame(table(method = d$Processing.Method), stringsAsFactors = FALSE)
      names(tab) <- c("method", "n")
      tab <- tab[order(-tab$n), ]
      tab$method <- factor(tab$method, levels = tab$method)
      tab$pct <- tab$n / sum(tab$n)
      ggplot(tab, aes(x = "", y = n, fill = method)) +
        geom_col(width = 1, colour = "white") +
        geom_text(aes(label = ifelse(pct >= 0.05, sprintf("%.0f%%", 100 * pct), "")),
                  position = position_stack(vjust = 0.5), size = 4.2, colour = "white") +
        coord_polar(theta = "y") +
        scale_fill_manual(values = cat_cols(nrow(tab)), name = NULL) +
        theme_void()
    })

    # ── Country scorecard: ring + progress-bar legend (shared helpers) ──────────
    output$country_ring <- renderPlot({
      s <- scored_slice()
      if (nrow(s) == 0) return(gg_no_data("No data for this selection."))
      scorecard_ring(scorecard_df(s))
    })
    output$country_legend <- renderUI({
      s <- scored_slice()
      if (nrow(s) == 0) return(NULL)
      scorecard_legend(scorecard_df(s))
    })

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
        labs(x = measure_label(m), y = "Coffees") + theme_coffee()
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
        labs(x = NULL, y = "Altitude (m)") + theme_coffee()
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
        labs(x = NULL, y = "Moisture (%)") + theme_coffee()
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

      op <- par(mar = c(1, 1, 1, 1)); on.exit(par(op))
      radarchart(radar_df, axistype = axt, seg = 4,
                 pcol = cols,
                 pfcol = c(adjustcolor(cols[1], alpha.f = 0.2),
                           adjustcolor(cols[2], alpha.f = 0)),
                 plwd = c(2.5, 2), plty = c(1, 2), pty = 16,
                 cglcol = COFFEE_COLS$grid, cglty = 1, cglwd = 0.8,
                 axislabcol = "#B9A88F", caxislabels = caxis, vlcex = 0.85)
      legend("topright", legend = rownames(mat), col = cols, lwd = 2,
             lty = c(1, 2), pch = 19, bty = "n", cex = 0.85)
    })
  })
}
