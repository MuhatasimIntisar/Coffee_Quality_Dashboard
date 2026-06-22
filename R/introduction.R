# Introduction tab
# ----------------
# Sets the scene: what the dataset is, its size, the time span, the factors
# recorded, and how the flavour score is built up. All figures are computed
# from the data so they never go stale.

introductionUI <- function(id) {
  ns <- NS(id)
  tagList(
    h2("Coffee Quality — an overview"),
    p(style = "max-width:760px; font-size:15px; color:#444;",
      "This dashboard explores the Coffee Quality Institute dataset: professional ",
      "cupping scores for green Arabica and Robusta coffees from around the world. ",
      "Each coffee is graded by certified tasters on nine sensory attributes and ",
      "described by where and how it was grown and processed. Use the tabs ",
      "above to explore origins on a ", strong("map"), ", dig into a country's ",
      strong("profile"), ", and ", strong("analyse"), " what drives quality."),

    # ── Headline figures ──────────────────────────────────────────────────────
    fluidRow(
      column(3, uiOutput(ns("kpi_coffees"))),
      column(3, uiOutput(ns("kpi_countries"))),
      column(3, uiOutput(ns("kpi_years"))),
      column(3, uiOutput(ns("kpi_attrs")))
    ),

    hr(),

    fluidRow(
      column(7,
        h4("Coffees graded per harvest year"),
        p("How many coffees in the dataset come from each harvest year."),
        plotOutput(ns("timeline"), height = "300px")
      ),
      column(5,
        h4("What's recorded for each coffee"),
        tags$ul(style = "font-size:14px; line-height:1.7;",
          tags$li(strong("Origin: "), "country and growing region"),
          tags$li(strong("Producer "), "and in-country grading partner"),
          tags$li(strong("Processing method: "), textOutput(ns("methods_inline"), inline = TRUE)),
          tags$li(strong("Altitude "), "of the farm (metres above sea level)"),
          tags$li(strong("Harvest year "), "and grading date"),
          tags$li(strong("Volume: "), "number of bags")
        ),
        h4("The flavour score"),
        p(style = "font-size:14px;",
          "The headline ", strong("Total Cup Points"), " (0–100) is the sum of nine ",
          "sensory attributes, each scored out of 10:"),
        uiOutput(ns("attr_chips"))
      )
    )
  )
}

introductionServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {

    scored <- data[!is.na(data$Total.Cup.Points) & data$Total.Cup.Points > 0, ]
    methods <- sort(unique(data$Processing.Method[data$Processing.Method != ""]))

    output$kpi_coffees <- renderUI(
      stat_card("Coffees graded", format(nrow(data), big.mark = ","),
                COFFEE_COLS$green))
    output$kpi_countries <- renderUI(
      stat_card("Countries of origin",
                length(unique(data$Country.of.Origin[data$Country.of.Origin != ""])),
                COFFEE_COLS$blue))
    output$kpi_years <- renderUI(
      stat_card("Harvest years",
                paste0(YEAR_RANGE[1], "–", YEAR_RANGE[2]), COFFEE_COLS$orange))
    output$kpi_attrs <- renderUI(
      stat_card("Sensory attributes", length(FLAVOR_ATTRS), COFFEE_COLS$purple))

    output$methods_inline <- renderText(paste(methods, collapse = ", "))

    output$attr_chips <- renderUI({
      div(style = "line-height:2.2;",
        lapply(FLAVOR_ATTRS, function(a)
          tags$span(style = paste0("display:inline-block; background:#f1e6ff;",
                                   "color:", COFFEE_COLS$purple, "; border-radius:6px;",
                                   "padding:3px 10px; font-size:13px; margin:0 6px 4px 0;"),
                    gsub("\\.", " ", a)))
      )
    })

    output$timeline <- renderPlot({
      df <- as.data.frame(table(year = data$harvest_year[!is.na(data$harvest_year)]))
      df$year <- as.integer(as.character(df$year))
      ggplot(df, aes(x = year, y = Freq)) +
        geom_col(fill = COFFEE_COLS$blue, width = 0.7) +
        geom_text(aes(label = Freq), vjust = -0.4, size = 3.5, colour = "#333333") +
        scale_x_continuous(breaks = df$year) +
        labs(x = "Harvest year", y = "Number of coffees") +
        theme_coffee()
    })
  })
}
