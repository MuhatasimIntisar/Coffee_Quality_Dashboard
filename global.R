library(shiny)
library(ggplot2)

# Register the UI's Google fonts for plots too, so chart text matches the rest
# of the dashboard (otherwise plot text stays the device default). Needs an
# internet connection on first launch to fetch the fonts.
library(showtext)
font_add_google("IBM Plex Sans", "IBM Plex Sans")
font_add_google("Space Grotesk", "Space Grotesk")
showtext_auto()

# global.R runs ONCE when the app starts, before ui.R / server.R.
# It loads + tidies the dataset and defines values shared by every tab
# (loaded once here so each module doesn't repeat the work).

# ── Load & tidy ─────────────────────────────────────────────────────────────
coffee <- read.csv("Group5_coffee.csv", stringsAsFactors = FALSE)

# Trim stray whitespace on the categorical columns we group/filter by.
for (col in c("Country.of.Origin", "Region", "Producer", "Processing.Method")) {
  coffee[[col]] <- trimws(coffee[[col]])
}

# Some values are entered with inconsistent capitalisation (e.g. "LA PLATA" vs
# "La Plata", "SEVERAL"/"Several"/"several"), which would otherwise split one
# producer into several groups in the treemap / ranking / finder. Collapse
# case-variants of the same value onto a single canonical spelling: the most
# frequent original, preferring a mixed-case form over ALL-CAPS or all-lower.
canonicalize_case <- function(x) {
  keep <- !is.na(x) & x != ""
  key  <- tolower(x)
  pick <- function(variants) {
    tab   <- sort(table(variants), decreasing = TRUE)
    cands <- names(tab)[tab == max(tab)]            # most frequent spelling(s)
    mixed <- cands[cands != toupper(cands) & cands != tolower(cands)]
    if (length(mixed)) mixed[1] else cands[1]
  }
  canon <- tapply(x[keep], key[keep], pick)         # named by lowercased key
  x[keep] <- canon[key[keep]]
  x
}

# Country feeds the map's coordinate lookup (fixed spellings), so leave it as is.
for (col in c("Region", "Producer", "Processing.Method")) {
  coffee[[col]] <- canonicalize_case(coffee[[col]])
}

# Harvest.Year is sometimes messy ("2013/2014", "Myanmar"): pull the first
# 4-digit year. The dataset spans 2011–2018.
coffee$harvest_year <- suppressWarnings(
  as.numeric(sub(".*?(\\d{4}).*", "\\1", coffee$Harvest.Year))
)

YEAR_RANGE <- range(coffee$harvest_year, na.rm = TRUE)

# ── Shared constants ────────────────────────────────────────────────────────

# The 9 sensory attributes scored for every coffee (used by the radar charts).
FLAVOR_ATTRS <- c("Aroma", "Flavor", "Aftertaste", "Acidity",
                  "Body", "Balance", "Uniformity", "Clean.Cup", "Sweetness")

# Default minimum sample size for "fair" rankings (exposed as a control).
MIN_SAMPLES_DEFAULT <- 5

# Approximate centroids (lat/lon) for every origin country in the dataset,
# keyed by the exact string in Country.of.Origin. Used to place bubbles on the
# Location map without needing heavyweight spatial packages.
COUNTRY_COORDS <- data.frame(
  country = c(
    "Brazil", "China", "Colombia", "Costa Rica", "Cote dIvoire", "El Salvador",
    "Ethiopia", "Guatemala", "Haiti", "Honduras", "Indonesia", "Kenya", "Laos",
    "Malawi", "Mexico", "Myanmar", "Nicaragua", "Panama", "Papua New Guinea",
    "Peru", "Philippines", "Rwanda", "Taiwan", "Tanzania, United Republic Of",
    "Thailand", "Uganda", "United States", "United States (Puerto Rico)", "Vietnam"),
  lat = c(-10.0, 35.0, 4.0, 10.0, 8.0, 13.8, 9.0, 15.5, 19.0, 15.0, -2.5, 0.0,
          18.0, -13.5, 23.0, 21.0, 12.9, 8.5, -6.0, -10.0, 13.0, -2.0, 23.7,
          -6.0, 15.0, 1.3, 39.5, 18.2, 16.0),
  lon = c(-55.0, 105.0, -72.0, -84.0, -5.5, -88.9, 39.0, -90.3, -72.4, -86.5,
          118.0, 38.0, 105.0, 34.0, -102.0, 96.0, -85.0, -80.0, 147.0, -76.0,
          122.0, 29.9, 121.0, 35.0, 101.0, 32.3, -98.0, -66.5, 108.0),
  stringsAsFactors = FALSE)
