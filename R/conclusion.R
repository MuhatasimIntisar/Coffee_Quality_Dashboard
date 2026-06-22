# Conclusion tab
# --------------
# Summarises what the dataset is dominated by (most prevalent origin, method,
# species, year) alongside the quality leaders, then a short set of takeaways.
# All figures computed from the data.

# Most frequent non-blank value of a column.
most_common <- function(x) {
  x <- x[!is.na(x) & x != ""]
  if (length(x) == 0) return("—")
  names(which.max(table(x)))
}

conclusionUI <- function(id) {
  ns <- NS(id)
  tagList(
    h2("Conclusions"),
    p("What the data is dominated by, and which origins and methods stand out."),

    h4("Most prevalent in the dataset"),
    fluidRow(
      column(3, uiOutput(ns("prev_country"))),
      column(3, uiOutput(ns("prev_method"))),
      column(3, uiOutput(ns("prev_year"))),
      column(3, uiOutput(ns("prev_species")))
    ),

    hr(),

    fluidRow(
      column(5,
        h4("Quality leaders"),
        p(style = "color:#888; font-size:13px;",
          "Countries with at least 5 graded coffees."),
        uiOutput(ns("lead_country")),
        uiOutput(ns("lead_method"))),
      column(7,
        h4("Most represented countries"),
        plotOutput(ns("topCountries"), height = "300px"))
    ),

    hr(),

    h4("Key takeaways"),
    uiOutput(ns("takeaways"))
  )
}

conclusionServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {

    scored <- data[!is.na(data$Total.Cup.Points) & data$Total.Cup.Points > 0, ]

    # Average score per country, restricted to >= 5 samples for fairness.
    country_avg <- reactive({
      tab <- summarise_by(scored, "Country.of.Origin")
      tab <- tab[tab$n_coffees >= 5, ]
      tab[order(-tab$avg_score), ]
    })
    best_method <- reactive({
      tab <- summarise_by(scored, "Processing.Method")
      tab <- tab[tab$n_coffees >= 5, ]
      tab$group[which.max(tab$avg_score)]
    })

    # ── Most prevalent ─────────────────────────────────────────────────────────
    output$prev_country <- renderUI(
      stat_card("Most coffees from", most_common(data$Country.of.Origin),
                COFFEE_COLS$blue))
    output$prev_method <- renderUI(
      stat_card("Most common processing", most_common(data$Processing.Method),
                COFFEE_COLS$orange))
    output$prev_year <- renderUI(
      stat_card("Most common harvest year",
                most_common(as.character(data$harvest_year)), COFFEE_COLS$purple))
    output$prev_species <- renderUI(
      stat_card("Dominant species", most_common(data$Species), COFFEE_COLS$green))

    # ── Quality leaders ────────────────────────────────────────────────────────
    output$lead_country <- renderUI({
      ca <- country_avg()
      stat_card("Highest average score",
                paste0(ca$group[1], " — ", sprintf("%.1f", ca$avg_score[1])),
                COFFEE_COLS$green)
    })
    output$lead_method <- renderUI(
      stat_card("Best-scoring processing method", best_method(), COFFEE_COLS$blue))

    # ── Most represented countries (by coffees graded) ─────────────────────────
    output$topCountries <- renderPlot({
      tab <- summarise_by(data, "Country.of.Origin")
      tab <- head(tab[order(-tab$n_coffees), ], 8)
      tab$group <- factor(tab$group, levels = rev(tab$group))
      ggplot(tab, aes(n_coffees, group)) +
        geom_col(fill = COFFEE_COLS$blue, width = 0.72) +
        geom_text(aes(label = n_coffees), hjust = -0.2, size = 3.4, colour = "#333") +
        scale_x_continuous(expand = expansion(mult = c(0, 0.1))) +
        labs(x = "Number of coffees graded", y = NULL) +
        theme_coffee()
    })

    # ── Takeaways (computed sentences) ──────────────────────────────────────────
    output$takeaways <- renderUI({
      ca   <- country_avg()
      top  <- most_common(data$Country.of.Origin)
      meth <- most_common(data$Processing.Method)
      tags$ul(style = "font-size:14px; line-height:1.8; max-width:820px;",
        tags$li(HTML(sprintf(
          "The dataset leans heavily on a few origins — <b>%s</b> contributes the most graded coffees, so country averages elsewhere rest on smaller samples.",
          top))),
        tags$li(HTML(sprintf(
          "<b>%s</b> is by far the most common processing method, and processing method shows only small differences in average cup score.",
          meth))),
        tags$li(HTML(sprintf(
          "On quality, <b>%s</b> leads among well-sampled countries (avg %.1f), and scores cluster tightly in the low-to-mid 80s overall.",
          ca$group[1], ca$avg_score[1]))),
        tags$li("Higher growing altitude is associated with modestly higher cup scores (see the Analysis tab).")
      )
    })
  })
}
