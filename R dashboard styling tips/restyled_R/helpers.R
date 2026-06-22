# Shared helpers
# ---------------
# Reusable UI components, a common ggplot2 theme, the radar charts, and the
# aggregation used by Location/Ranking. Everything in R/ is auto-sourced, so
# these are visible to every module. Pure helpers (no module state).
#
# RESTYLE NOTES: only the *aesthetic* pieces changed — COFFEE_COLS, LATTE,
# CAT_COLS, stat_card(), theme_coffee(), gg_no_data(). All keys/signatures are
# unchanged, so the modules need NO edits. Logic (summarise_by, METRIC_*,
# MEASURE_*, draw_radar, prod_radar_compare) is untouched.

# ── Brand palette (Coffee) ────────────────────────────────────────────────────
# Keys kept (blue/green/purple/orange/grey/grid) so modules need no changes —
# each now maps to a refined, higher-contrast coffee tone.
COFFEE_COLS <- list(
  blue   = "#8A5A2B", blue_lt  = "#C68642",   # roasted brown / caramel (primary data)
  green  = "#2E8B74", green_dk = "#1F6B58",   # fresh green
  purple = "#3A2417", orange   = "#C68642",   # espresso ink / caramel (highlight)
  grey   = "#9C8F7E", grid     = "#EFE4D2"    # muted brown / soft grid
)

# Coffee surface tones reused by themes/cards.
LATTE <- list(base = "#F7F1E7", mantle = "#F1E4CE", surface = "#FFFFFF",
              line = "#EADDCB", text = "#2B2018", subtext = "#6F5C49")

# Distinct categorical hues for multi-slice charts. Warm-led but spread across
# the wheel so methods/regions are actually TELL-APART on the scatter & pie.
# Last colour ("#9C8F7E") is the muted grey used for "Other".
CAT_COLS <- c("#8A5A2B",  # roast
              "#2E8B74",  # green
              "#C68642",  # caramel
              "#3E5C76",  # slate blue
              "#B5651D",  # clay
              "#7A6F9B",  # muted plum
              "#5C8D7B",  # sage
              "#A23E48",  # brick
              "#9C8F7E")  # grey (Other)

# ── Stat / KPI card ─────────────────────────────────────────────────────────
# Redesigned: a clean white card with a short accent rule on top, mono label,
# and a large display-font value — instead of the old left-border tan tab.
stat_card <- function(label, value, accent = COFFEE_COLS$blue) {
  div(
    style = paste0("background:", LATTE$surface, "; border:1px solid ", LATTE$line, ";",
                   "border-radius:14px; padding:16px 18px; margin-bottom:14px;",
                   "box-shadow:0 1px 2px rgba(58,36,23,.05);"),
    tags$span(style = paste0("display:block; width:26px; height:3px; border-radius:2px;",
                             "background:", accent, "; margin-bottom:12px;")),
    tags$p(style = paste0("font-family:'IBM Plex Mono',monospace; font-size:11px;",
                          "letter-spacing:.08em; text-transform:uppercase;",
                          "color:", LATTE$subtext, "; margin:0 0 6px;"), label),
    tags$p(style = paste0("font-family:'Space Grotesk',sans-serif; font-size:26px;",
                          "font-weight:600; line-height:1; margin:0; color:", LATTE$text, ";"),
           value)
  )
}

# ── Shared ggplot2 theme ────────────────────────────────────────────────────
# Tighter, less generic: drop x gridlines, lighten y gridlines, kill ticks,
# stronger title hierarchy, more breathing room. Pass family = "IBM Plex Sans"
# once you've registered the Google fonts for plots (see global.R note); left
# NULL by default so it never errors if the font isn't registered.
theme_coffee <- function(base_size = 13, family = NULL) {
  theme_minimal(base_size = base_size, base_family = family %||% "") +
    theme(
      text             = element_text(colour = LATTE$text),
      plot.title       = element_text(face = "bold", size = base_size + 3,
                                      colour = LATTE$text, margin = margin(b = 10)),
      plot.subtitle    = element_text(colour = LATTE$subtext, size = base_size - 1),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(colour = "#EFE4D2", linewidth = 0.5),
      axis.title       = element_text(colour = LATTE$subtext, size = base_size - 2),
      axis.text        = element_text(colour = LATTE$subtext),
      axis.ticks       = element_blank(),
      legend.position  = "bottom",
      legend.title     = element_text(size = base_size - 2, colour = LATTE$subtext),
      legend.text      = element_text(size = base_size - 2),
      plot.background  = element_rect(fill = NA, colour = NA),
      panel.background = element_rect(fill = NA, colour = NA),
      plot.margin      = margin(10, 14, 8, 8)
    )
}

# Tiny null-coalescing helper (in case rlang's %||% isn't attached).
`%||%` <- function(a, b) if (is.null(a)) b else a

# Empty-state ggplot placeholder, so charts degrade gracefully on no data.
gg_no_data <- function(msg = "No data for this selection.") {
  ggplot() +
    annotate("text", x = 0, y = 0, label = msg, size = 5, colour = LATTE$subtext) +
    theme_void()
}

# ── Metric aggregation ──────────────────────────────────────────────────────
# Summarise a (filtered) data frame by a grouping column into the four metrics
# the Location map and Ranking tab share. Returns one row per group.
summarise_by <- function(df, group_col) {
  df <- df[!is.na(df[[group_col]]) & df[[group_col]] != "", ]
  if (nrow(df) == 0) {
    return(data.frame(group = character(0), n_coffees = integer(0),
                      avg_score = numeric(0), avg_altitude = numeric(0),
                      total_bags = numeric(0), n_producers = integer(0),
                      n_regions = integer(0),
                      stringsAsFactors = FALSE))
  }
  parts <- split(df, df[[group_col]])
  out <- lapply(names(parts), function(g) {
    p <- parts[[g]]
    alt <- p$altitude_mean_meters[!is.na(p$altitude_mean_meters) &
                                  p$altitude_mean_meters > 0 &
                                  p$altitude_mean_meters < 4000]
    data.frame(
      group        = g,
      n_coffees    = nrow(p),
      avg_score    = mean(p$Total.Cup.Points, na.rm = TRUE),
      avg_altitude = if (length(alt)) mean(alt) else NA_real_,
      total_bags   = sum(p$Number.of.Bags, na.rm = TRUE),
      n_producers  = length(unique(p$Producer[p$Producer != ""])),
      n_regions    = length(unique(p$Region[p$Region != ""])),
      stringsAsFactors = FALSE)
  })
  do.call(rbind, out)
}

# Human-readable labels for the metric keys used across tabs.
METRIC_LABELS <- c(avg_score    = "Average cup score",
                   avg_altitude = "Average altitude (m)",
                   n_coffees    = "Number of coffees",
                   total_bags   = "Total bags",
                   n_producers  = "Number of producers")

# Oriented for selectInput(choices=): names (labels) are shown, values (keys)
# are returned as input$metric. (A named vector shows names, returns values.)
METRIC_CHOICES <- setNames(names(METRIC_LABELS), unname(METRIC_LABELS))

# Measure choices used by the Analysis tab: Total Cup Points + the 9 sensory
# attributes. Names are shown in dropdowns; values are the column names.
MEASURE_CHOICES <- setNames(c("Total.Cup.Points", FLAVOR_ATTRS),
                            c("Total Cup Points", gsub("\\.", " ", FLAVOR_ATTRS)))
measure_label <- function(m) names(MEASURE_CHOICES)[match(m, MEASURE_CHOICES)]

# ── Radar charts (base graphics) ────────────────────────────────────────────
# A radar is awkward in ggplot2, so we keep these hand-drawn base-R helpers as
# the dashboard's "we went beyond ggplot2" piece. draw_radar() plots one
# profile; prod_radar_compare() overlays several for direct comparison.
# (Colours come from COFFEE_COLS, so they restyle automatically.)

draw_radar <- function(scores, title = "") {
  n       <- length(scores)
  angles  <- seq(0, 2 * pi, length.out = n + 1)[-(n + 1)]
  min_val <- 6; max_val <- 10
  norm    <- pmax(0, pmin(1, (scores - min_val) / (max_val - min_val)))
  px <- norm * cos(angles - pi/2); py <- norm * sin(angles - pi/2)

  par(mar = c(1, 1, 2, 1))
  plot(0, 0, type = "n", xlim = c(-1.6, 1.6), ylim = c(-1.6, 1.6), asp = 1,
       axes = FALSE, xlab = "", ylab = "", main = title, cex.main = 0.95)
  for (r in c(0.25, 0.5, 0.75, 1.0)) {
    gx <- r * cos(seq(0, 2*pi, length.out = 200) - pi/2)
    gy <- r * sin(seq(0, 2*pi, length.out = 200) - pi/2)
    lines(gx, gy, col = COFFEE_COLS$grid, lwd = 0.8)
    text(0, r + 0.03, sprintf("%.1f", min_val + r * (max_val - min_val)),
         cex = 0.5, col = "#B9A88F")
  }
  for (i in seq_len(n))
    lines(c(0, cos(angles[i] - pi/2)), c(0, sin(angles[i] - pi/2)),
          col = COFFEE_COLS$grid, lwd = 0.8)
  polygon(c(px, px[1]), c(py, py[1]),
          col = adjustcolor(COFFEE_COLS$green, 0.2),
          border = COFFEE_COLS$green, lwd = 2)
  points(px, py, pch = 21, bg = COFFEE_COLS$green, col = "white", cex = 1.6, lwd = 1.5)
  for (i in seq_len(n)) {
    lab <- gsub("\\.", " ", names(scores)[i])
    cx  <- cos(angles[i] - pi/2)
    adj_x <- if (cx < -0.1) 1 else if (cx > 0.1) 0 else 0.5
    text(1.32 * cos(angles[i] - pi/2), 1.32 * sin(angles[i] - pi/2),
         lab, cex = 0.72, col = LATTE$text, adj = c(adj_x, 0.5))
  }
}

prod_radar_compare <- function(series, cols) {
  attrs   <- names(series[[1]])
  n       <- length(attrs)
  angles  <- seq(0, 2 * pi, length.out = n + 1)[-(n + 1)]
  min_val <- 6; max_val <- 10

  par(mar = c(1, 1, 2, 1))
  plot(0, 0, type = "n", xlim = c(-1.7, 1.7), ylim = c(-1.7, 1.7), asp = 1,
       axes = FALSE, xlab = "", ylab = "")
  for (r in c(0.25, 0.5, 0.75, 1.0)) {
    gx <- r * cos(seq(0, 2*pi, length.out = 200) - pi/2)
    gy <- r * sin(seq(0, 2*pi, length.out = 200) - pi/2)
    lines(gx, gy, col = COFFEE_COLS$grid, lwd = 0.8)
    text(0, r + 0.03, sprintf("%.1f", min_val + r * (max_val - min_val)),
         cex = 0.5, col = "#B9A88F")
  }
  for (i in seq_len(n))
    lines(c(0, cos(angles[i] - pi/2)), c(0, sin(angles[i] - pi/2)),
          col = COFFEE_COLS$grid, lwd = 0.8)
  for (s in seq_along(series)) {
    norm <- pmax(0, pmin(1, (series[[s]] - min_val) / (max_val - min_val)))
    px <- norm * cos(angles - pi/2); py <- norm * sin(angles - pi/2)
    polygon(c(px, px[1]), c(py, py[1]),
            col = adjustcolor(cols[s], 0.18), border = cols[s], lwd = 2)
    points(px, py, pch = 21, bg = cols[s], col = "white", cex = 1.3, lwd = 1.2)
  }
  for (i in seq_len(n)) {
    lab   <- gsub("\\.", " ", attrs[i])
    cx    <- cos(angles[i] - pi/2)
    adj_x <- if (cx < -0.1) 1 else if (cx > 0.1) 0 else 0.5
    text(1.32 * cos(angles[i] - pi/2), 1.32 * sin(angles[i] - pi/2),
         lab, cex = 0.72, col = LATTE$text, adj = c(adj_x, 0.5))
  }
  legend("topright", legend = names(series), col = cols,
         lwd = 2, pch = 19, bty = "n", cex = 0.85)
}
