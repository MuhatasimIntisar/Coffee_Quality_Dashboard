# Summary tab
# -----------
# The headline discoveries: what dominates the dataset (biggest producing
# country, most productive year, most used method), which perform best (highest-
# scoring country, year and method), then a written round-up of the deeper
# findings (altitude, moisture, flavour drivers, defects). Figures computed from
# the data; the five top-scoring coffees close the tab.

library(DT)

# Most frequent non-blank value of a column.
most_common <- function(x) {
  x <- x[!is.na(x) & x != ""]
  if (length(x) == 0) return("—")
  names(which.max(table(x)))
}

conclusionUI <- function(id) {
  ns <- NS(id)
  tagList(
    h2("Summary"),

    h4("Biggest and most common"),
    fluidRow(
      column(4, uiOutput(ns("prev_country"))),
      column(4, uiOutput(ns("prev_year"))),
      column(4, uiOutput(ns("prev_method")))
    ),

    hr(),

    h4("Highest performing"),
    p(style = "color:#888; font-size:13px;",
      "By average cup score, among groups with enough graded coffees to be fair."),
    fluidRow(
      column(4, uiOutput(ns("lead_country"))),
      column(4, uiOutput(ns("lead_year"))),
      column(4, uiOutput(ns("lead_method")))
    ),

    hr(),

    h4("What we found"),
    uiOutput(ns("takeaways")),

    hr(),

    h4("Coffee finder"),
    p(style = "color:#7B4F2E; font-size:13px;",
      "Find a specific subset of coffees — use the search box or the per-column ",
      "filters to narrow by country, region, method, or an altitude / moisture / ",
      "score range."),
    DTOutput(ns("finder"))
  )
}

conclusionServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {

    scored <- data[!is.na(data$Total.Cup.Points) & data$Total.Cup.Points > 0, ]

    # Average score per country (>= 5 coffees for fairness).
    country_avg <- reactive({
      tab <- summarise_by(scored, "Country.of.Origin")
      tab <- tab[tab$n_coffees >= 5, ]
      tab[order(-tab$avg_score), ]
    })
    # Best-scoring processing method (>= 5 coffees).
    best_method <- reactive({
      tab <- summarise_by(scored, "Processing.Method")
      tab <- tab[tab$n_coffees >= 5, ]
      tab$group[which.max(tab$avg_score)]
    })
    # Highest-scoring harvest year (years with >= 20 graded coffees, to be fair).
    best_year <- reactive({
      s <- scored[!is.na(scored$harvest_year), ]
      m <- tapply(s$Total.Cup.Points, s$harvest_year, mean)
      n <- table(s$harvest_year)
      keep <- names(n)[n >= 20]
      if (length(keep) == 0) return(NULL)
      m <- m[keep]
      list(year = names(which.max(m)), score = max(m))
    })

    # ── Biggest / most common ──────────────────────────────────────────────────
    output$prev_country <- renderUI(
      stat_card("Biggest producing country", most_common(data$Country.of.Origin),
                COFFEE_COLS$blue))
    output$prev_year <- renderUI(
      stat_card("Most productive year",
                most_common(as.character(data$harvest_year)), COFFEE_COLS$orange))
    output$prev_method <- renderUI(
      stat_card("Most used method", most_common(data$Processing.Method),
                COFFEE_COLS$purple))

    # ── Highest performing ─────────────────────────────────────────────────────
    output$lead_country <- renderUI({
      ca <- country_avg()
      stat_card("Highest-scoring country",
                paste0(ca$group[1], " — ", sprintf("%.1f", ca$avg_score[1])),
                COFFEE_COLS$green)
    })
    output$lead_year <- renderUI({
      by <- best_year()
      v  <- if (is.null(by)) "—" else paste0(by$year, " — ", sprintf("%.1f", by$score))
      stat_card("Highest-scoring year", v, COFFEE_COLS$green)
    })
    output$lead_method <- renderUI(
      stat_card("Best-scoring method", best_method(), COFFEE_COLS$green))

    # ── Findings round-up (the deeper discoveries) ──────────────────────────────
    output$takeaways <- renderUI({
      ca   <- country_avg()
      meth <- most_common(data$Processing.Method)
      tags$ul(style = "font-size:14px; line-height:1.8; max-width:880px;",
        tags$li(HTML(sprintf(
          "<b>Altitude lifts quality.</b> Higher-grown coffees score modestly but consistently better across nearly every attribute — strongest on acidity and flavour. %s leads the well-sampled countries (avg %.1f) and is among the highest-altitude origins.",
          ca$group[1], ca$avg_score[1]))),
        tags$li(HTML(
          "<b>Altitude and moisture interact.</b> The best scores sit at high altitude with mid-range moisture (~11–13%); the worst are low-altitude, very wet beans. So it isn't altitude alone — there's a sweet spot.")),
        tags$li(HTML(
          "<b>Aftertaste and flavour drive the score.</b> Of the nine attributes, aftertaste and flavour correlate most strongly with the total; sweetness, clean cup and uniformity least — because almost every coffee scores near-perfect on those, so they don't separate the field.")),
        tags$li(HTML(
          "<b>Quality crashes, it doesn't drift.</b> Low totals come from a single category collapsing — a clean-cup or sweetness defect — rather than gradually worse taste. Those three attributes act as pass/fail switches.")),
        tags$li(HTML(sprintf(
          "<b>Processing matters little.</b> <b>%s</b> is by far the most common method, and average scores differ only slightly across methods.",
          meth))),
        tags$li(HTML(
          "<b>Mind the sample.</b> The data leans on a few origins and older harvest years, so averages elsewhere rest on smaller samples."))
      )
    })

    # ── Coffee finder: per-column filterable table for picking a subset ─────────
    output$finder <- renderDT({
      d <- scored
      tab <- data.frame(
        Country  = d$Country.of.Origin, Region = d$Region, Producer = d$Producer,
        Year     = d$harvest_year, Method = d$Processing.Method,
        Altitude = round(d$altitude_mean_meters),
        Moisture = round(d$Moisture * 100, 1),
        Score    = round(d$Total.Cup.Points, 2),
        Aroma    = d$Aroma, Flavor = d$Flavor, Acidity = d$Acidity, Body = d$Body,
        stringsAsFactors = FALSE, check.names = FALSE)
      datatable(tab, rownames = FALSE, filter = "top",
                options = list(pageLength = 10, order = list()),
                class = "stripe hover compact")
    })
  })
}
