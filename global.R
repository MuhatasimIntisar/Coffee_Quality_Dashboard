library(shiny)
library(ggplot2)

# Plot text uses the graphics device's default (system) font, so nothing needs
# to be downloaded — the dashboard runs offline with no external font fetch.

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

# Number of coffees carrying a valid overall score (the basis for the scorecard).
N_SCORED <- sum(!is.na(coffee$Total.Cup.Points) & coffee$Total.Cup.Points > 0)

# ── Shared constants ────────────────────────────────────────────────────────

# The 9 sensory attributes scored for every coffee (used by the radar charts).
FLAVOR_ATTRS <- c("Aroma", "Flavor", "Aftertaste", "Acidity",
                  "Body", "Balance", "Uniformity", "Clean.Cup", "Sweetness")

# Default minimum sample size for "fair" rankings (exposed as a control).
MIN_SAMPLES_DEFAULT <- 5
