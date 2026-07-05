# Shared helpers
# ---------------
# Reusable UI components, a common ggplot2 theme, the radar charts, and the
# aggregation used by Location/Ranking. Everything in R/ is auto-sourced, so
# these are visible to every module. Pure helpers (no module state).
#
# RESTYLE NOTES: only the *aesthetic* pieces changed — COFFEE_COLS, LATTE,
# CAT_COLS, stat_card(), theme_coffee(), gg_no_data(). All keys/signatures are
# unchanged, so the modules need NO edits. Logic (summarise_by, METRIC_*,
# MEASURE_*) is untouched.

# ── Brand palette (Coffee) ────────────────────────────────────────────────────
# Keys kept (blue/green/purple/orange/grey/grid) so modules need no changes —
# each now maps to a refined, higher-contrast coffee tone.
COFFEE_COLS <- list(
  blue   = "#9C5A20", blue_lt  = "#C68642",   # roasted brown / caramel (primary data)
  green  = "#0E8A63", green_dk = "#0B6B4D",   # fresh green
  purple = "#3A2417", orange   = "#AE7A0F",   # espresso ink / golden roast (highlight)
  grey   = "#9C8F7E", grid     = "#EFE4D2"    # muted brown / soft grid
)

# Coffee surface tones reused by themes/cards.
LATTE <- list(base = "#F7F1E7", mantle = "#F1E4CE", surface = "#FFFFFF",
              line = "#EADDCB", text = "#2B2018", subtext = "#6F5C49")

# Categorical hues for multi-series charts — a warm coffee palette, validated
# for lightness band, chroma floor, colour-vision separation and 3:1 contrast
# against the cream surface (#F7F1E7). Assigned in this fixed order.
CAT_COLS <- c("#9C5A20",  # roast brown
              "#0E8A63",  # fresh green
              "#AE7A0F",  # golden honey
              "#B04A38",  # baked clay
              "#3E7CB1",  # lake blue
              "#A03A64")  # berry

# Pick n distinct categorical colours: straight from CAT_COLS when there are
# enough, only interpolating when more than the palette length are needed.
cat_cols <- function(n) {
  if (n <= length(CAT_COLS)) CAT_COLS[seq_len(n)] else colorRampPalette(CAT_COLS)(n)
}

# Shared warm sequential fill for heatmaps / choropleths (cream -> espresso).
# A function, so each plot gets a fresh scale (a reused scale object trains its
# colour range across plots and mis-maps). Pass the legend title in `name`.
warm_fill <- function(name = waiver()) {
  scale_fill_gradientn(
    colours  = c("#F7F1E7", "#E8C99A", "#C68642", "#8A5A2B", "#3A2417"),
    na.value = "#EAE0D0", name = name)
}

# ── Average scorecard (ring + progress-bar legend) ──────────────────────────
# Shared by Overview and Profile. Build the ten-component means from a set of
# scored coffees, then render a clean ring (total in the middle) and a legend
# with a colour swatch, name, /10 progress bar and value for each component.
scorecard_df <- function(scored) {
  comps  <- c(FLAVOR_ATTRS, "Cupper.Points")
  vals   <- vapply(comps, function(c) mean(scored[[c]], na.rm = TRUE), numeric(1))
  labels <- ifelse(comps == "Cupper.Points", "Grader overall", gsub("\\.", " ", comps))
  data.frame(label = factor(labels, levels = labels), val = as.numeric(vals),
             col = cat_cols(length(comps)), stringsAsFactors = FALSE)
}

scorecard_ring <- function(df) {
  df$ymax <- cumsum(df$val); df$ymin <- c(0, head(df$ymax, -1))
  total <- sum(df$val)
  ggplot(df) +
    geom_rect(aes(ymin = ymin, ymax = ymax, xmin = 3, xmax = 4, fill = label),
              colour = "white", linewidth = 0.6) +
    annotate("text", x = 0, y = 0, label = sprintf("%.0f", total),
             size = 16, fontface = "bold", colour = LATTE$text) +
    annotate("text", x = 1.15, y = total / 2, label = "out of 100",
             size = 4.4, colour = LATTE$subtext) +
    coord_polar(theta = "y") + xlim(c(0, 4)) +
    scale_fill_manual(values = setNames(df$col, levels(df$label)), guide = "none") +
    theme_void()
}

scorecard_legend <- function(df, ref = NULL) {
  # ref: optional vector of comparison values (same order/length as df$val) — the
  # dataset-wide component averages — drawn as a vertical tick on each bar.
  fmt <- function(v) sub("\\.0$", "", sprintf("%.1f", v))
  rows <- lapply(seq_len(nrow(df)), function(i) {
    pct <- max(0, min(100, df$val[i] / 10 * 100))
    marker <- if (!is.null(ref))
      div(title = sprintf("Dataset average %s", fmt(ref[i])),
          style = paste0("position:absolute; top:-3px; bottom:-3px; width:2px;",
                         " left:", max(0, min(100, ref[i] / 10 * 100)), "%;",
                         " background:#2B2018; opacity:.55;"))
    div(style = "display:flex; align-items:center; gap:10px; margin-bottom:9px;",
        tags$span(style = paste0("width:14px; height:14px; border-radius:3px; flex:none;",
                                 " background:", df$col[i], ";")),
        tags$span(style = "width:120px; flex:none; font-size:15px; color:#2B2018;",
                  as.character(df$label[i])),
        div(style = paste0("position:relative; flex:1; height:10px; border-radius:5px;",
                           " background:#EFE4D2;"),
            div(style = paste0("width:", pct, "%; height:100%; border-radius:5px;",
                               " background:", df$col[i], ";")),
            marker),
        tags$span(style = "width:40px; text-align:right; flex:none; font-size:15px; color:#2B2018;",
                  fmt(df$val[i])))
  })
  note <- if (!is.null(ref))
    div(style = "font-size:12px; color:#6F5C49; margin-top:8px;",
        "The vertical line on each bar marks the dataset-wide average.")
  div(rows, note)
}

scorecard_card <- function(ring, legend, n = NULL) {
  # n may be NULL (no count), a number (static count), or a shiny tag such as a
  # textOutput (a dynamic, reactive count — used by the per-country Profile card).
  base <- "Total out of 100, split into its ten components"
  sub <- if (is.null(n)) base
         else if (is.numeric(n))
           paste0(base, " · ", format(n, big.mark = ","), " coffees evaluated")
         else list(base, " · ", n)
  card(
    card_header(
      div(style = "display:flex; justify-content:space-between; align-items:baseline; gap:12px;",
          tags$span("Average scorecard"),
          tags$span(style = "font-weight:400; color:#6F5C49; font-size:14px;",
                    sub))),
    card_body(
      layout_columns(
        col_widths = c(5, 7),
        div(style = "display:flex; align-items:center; justify-content:center;", ring),
        legend))
  )
}

# ── Stat / KPI card ─────────────────────────────────────────────────────────
# Redesigned: a clean white card with a short accent rule on top, mono label,
# and a large display-font value — instead of the old left-border tan tab.
stat_card <- function(label, value, accent = COFFEE_COLS$blue, height = NULL) {
  div(
    style = paste0("background:", LATTE$surface, "; border:1px solid ", LATTE$line, ";",
                   "border-radius:14px; padding:16px 18px; margin-bottom:14px;",
                   "box-shadow:0 1px 2px rgba(58,36,23,.05);",
                   if (!is.null(height))
                     paste0("height:", height, "; box-sizing:border-box;")),
    tags$span(style = paste0("display:block; width:26px; height:3px; border-radius:2px;",
                             "background:", accent, "; margin-bottom:12px;")),
    tags$p(style = paste0("font-family:ui-monospace,Consolas,'Liberation Mono',monospace; font-size:11px;",
                          "letter-spacing:.08em; text-transform:uppercase;",
                          "color:", LATTE$subtext, "; margin:0 0 6px;"), label),
    tags$p(style = paste0("font-family:system-ui,-apple-system,'Segoe UI',Roboto,Helvetica,Arial,sans-serif; font-size:26px;",
                          "font-weight:600; line-height:1; margin:0; color:", LATTE$text, ";"),
           value)
  )
}

# ── Shared ggplot2 theme ────────────────────────────────────────────────────
# Tighter, less generic: drop x gridlines, lighten y gridlines, kill ticks,
# stronger title hierarchy, more breathing room. Chart text uses the device's
# default (system) font — no font needs to be downloaded or registered.
theme_coffee <- function(base_size = 15, family = "") {
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

# Fixed axis limits so ranges stay constant across selections (easier to read):
#   Total Cup Points 60-90; clean cup & sweetness 0-10; other sensory attributes
#   5-10; altitude 0-4000 m; moisture 5-20 %.
measure_limits <- function(m)
  if (identical(m, "Total.Cup.Points")) c(60, 90)
  else if (m %in% c("Clean.Cup", "Sweetness")) c(0, 10)
  else c(5, 10)
ALT_LIMITS   <- c(0, 4000)
MOIST_LIMITS <- c(5, 20)

# Shared altitude / moisture bands so every tab groups coffees the same way
# (used by the Attributing-Factors heatmap and the Conclusion "what scores
# highest" cards). Change the breaks here and every tab updates together.
ALT_BREAKS        <- c(0, 1000, 1250, 1500, 1750, 2000, Inf)
ALT_BAND_LABELS   <- c("<1000", "1000–1250", "1250–1500", "1500–1750",
                       "1750–2000", "2000+")
MOIST_BREAKS      <- c(0, 0.10, 0.11, 0.12, 0.13, Inf)
MOIST_BAND_LABELS <- c("<10%", "10–11%", "11–12%", "12–13%", "13%+")
alt_band   <- function(x) cut(x, ALT_BREAKS,   ALT_BAND_LABELS,   right = FALSE)
moist_band <- function(x) cut(x, MOIST_BREAKS, MOIST_BAND_LABELS, right = FALSE)

# Radar charts are drawn with the fmsb package directly inside the Attributing-
# Factors and Profile modules, so the old hand-drawn base-R radar helpers that
# used to live here have been removed.
