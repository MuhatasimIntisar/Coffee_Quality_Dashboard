# Summary tab — the whole story on one page
# ------------------------------------------
# Headline discoveries styled as a gentle read: what dominates the dataset,
# what performs best, then the written round-up of the deeper findings. The
# old coffee-finder table now lives on in a friendlier form as the Sensory
# Analysis tab's coffee builder.

# Most frequent non-blank value of a column.
most_common <- function(x) {
  x <- x[!is.na(x) & x != ""]
  if (length(x) == 0) return("—")
  names(which.max(table(x)))
}

# A styled section heading: tiny mono kicker above a clean title.
sum_heading <- function(kicker, title) {
  div(style = "margin:6px 0 14px;",
      div(style = paste0("font-family:ui-monospace,Consolas,monospace; font-size:11px;",
                         "letter-spacing:.16em; text-transform:uppercase; color:#9C5A20;",
                         "margin-bottom:4px;"), kicker),
      h4(style = "margin:0;", title))
}

conclusionUI <- function(id) {
  ns <- NS(id)
  tagList(
      h2("The story so far"),
      p(style = "max-width:860px; font-size:17px; color:#444; line-height:1.6;",
        "You have spun the globe, popped the flavour wheels and maybe even built ",
        "your own cup. Pour yourself something warm. Here is everything this ",
        "data has been quietly telling us, gathered in one place."),

      sum_heading("The headliners", "Biggest and most common"),
      fluidRow(
        column(4, uiOutput(ns("prev_country"))),
        column(4, uiOutput(ns("prev_year"))),
        column(4, uiOutput(ns("prev_method")))
      ),

      hr(),

      sum_heading("The podium", "Highest performing"),
      p(style = "color:#888; font-size:13px;",
        "By average cup score, among groups with enough graded coffees to be fair."),
      fluidRow(
        column(4, uiOutput(ns("lead_country"))),
        column(4, uiOutput(ns("lead_year"))),
        column(4, uiOutput(ns("lead_method")))
      ),

      hr(),

      sum_heading("The insights", "Six things this data taught us"),
      uiOutput(ns("takeaways")),

      hr(),

      sum_heading("One last sip", "What to remember"),
      p(style = "max-width:860px; font-size:16px; color:#2B2018; line-height:1.7;",
        "Great coffee is grown before it is made. Height, care and craft on a ",
        "distant hillside decide most of what you taste at home. So next time a ",
        "bag tells you its farm sits high in the mountains of Ethiopia or ",
        "Guatemala, that is not marketing. That is the single best predictor in ",
        "this entire dataset. Happy brewing.")
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
      stat_card("Highest scoring country",
                paste0(ca$group[1], " at ", sprintf("%.1f", ca$avg_score[1])),
                COFFEE_COLS$green)
    })
    output$lead_year <- renderUI({
      by <- best_year()
      v  <- if (is.null(by)) "—" else paste0(by$year, " at ", sprintf("%.1f", by$score))
      stat_card("Highest scoring year", v, COFFEE_COLS$green)
    })
    output$lead_method <- renderUI(
      stat_card("Best scoring method", best_method(), COFFEE_COLS$green))

    # ── Findings round-up: six highlight cards ────────────────────────────────
    # (Card styling lives in ui.R: .takeaway-card.)
    output$takeaways <- renderUI({
      ca   <- country_avg()
      meth <- most_common(data$Processing.Method)

      tk_card <- function(i, accent, title, body) {
        div(class = "takeaway-card",
            style = sprintf("--tk-accent:%s; --i:%d;", accent, i - 1),
            div(class = "takeaway-num", sprintf("No. %d", i)),
            div(class = "takeaway-title", title),
            p(class = "takeaway-body", body))
      }

      div(class = "takeaway-grid",
        tk_card(1, CAT_COLS[2], "The mountains make the magic",
          sprintf("The higher a coffee grows, the better it tends to taste. Cool, thin mountain air slows the cherry's ripening and concentrates its sweetness and flavour. %s, home to some of the highest farms here, leads the well-sampled countries with an average of %.1f.",
                  ca$group[1], ca$avg_score[1])),
        tk_card(2, CAT_COLS[5], "Great coffee is a duet",
          "Height alone is not the whole song. The very best cups pair high altitude with beans holding a comfortable 11 to 13 percent moisture, while low-grown, very wet beans sit at the bottom of the pile. It is a sweet spot, not a single dial."),
        tk_card(3, CAT_COLS[1], "The finish tells the truth",
          "Of the nine things tasters score, aftertaste and flavour follow the final mark most faithfully. If a coffee still tastes wonderful ten seconds after you swallow, chances are it scored beautifully everywhere else too."),
        tk_card(4, CAT_COLS[4], "Bad coffee fails loudly",
          "Low scores almost never come from everything tasting slightly dull. They come from one thing going properly wrong, usually a defect in cleanliness or sweetness. Quality does not fade politely. It trips."),
        tk_card(5, CAT_COLS[3], "Do not sweat the process",
          sprintf("%s is by far the most common way of preparing beans, but honestly, the method barely moves the needle. The differences between washed, natural and honey coffees are a gentle nudge, not a transformation.", meth)),
        tk_card(6, CAT_COLS[6], "Enjoy the stars, mind the fine print",
          "A few origins and older harvests carry most of this data, so the smaller origins' dazzling averages rest on just a handful of cups. Savour their promise, and keep one eyebrow gently raised.")
      )
    })
  })
}
