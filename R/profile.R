# Profile tab
# -----------
# Granular detail on a single country over a chosen harvest-year range. Top
# controls (country + year range) mirror the Global tab. Below: a breakdown pie
# the user can switch between processing methods / regions / producers, a score
# histogram, an altitude boxplot, and the flavour-profile radar.
#
# Reacts to the shared `nav` bus (see server.R): clicking a country on the
# Global tab opens this tab with that country preselected.

# Mean of the 9 sensory attributes for a data frame.
profile_means <- function(df) {
  setNames(sapply(FLAVOR_ATTRS, function(a) mean(df[[a]], na.rm = TRUE)), FLAVOR_ATTRS)
}

# Pie chart of a categorical column: top N categories by count, the rest pooled
# into "Other". Slice labels show the share when it's big enough to read.
count_pie <- function(values, n) {
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
  base      <- colorRampPalette(CAT_COLS)(max(1, k - has_other))
  pal       <- if (has_other) c(base, "#9ca0b0") else base

  ggplot(tab, aes(x = "", y = n, fill = cat)) +
    geom_col(width = 1, colour = "white", linewidth = 0.3) +
    coord_polar(theta = "y") +
    geom_text(aes(label = ifelse(pct >= 0.05, sprintf("%.0f%%", 100 * pct), "")),
              position = position_stack(vjust = 0.5), size = 3.2, colour = "white") +
    scale_fill_manual(values = setNames(pal, levels(tab$cat)), name = NULL) +
    theme_void(base_size = 12) +
    theme(legend.position = "right", legend.text = element_text(colour = LATTE$text))
}

profileUI <- function(id) {
  ns <- NS(id)
  tagList(
    h2("Profile: country deep dive"),
    p("Pick a country and a harvest-year range to see that slice in depth — how ",
      "it breaks down, how its scores are distributed, its growing altitude, and ",
      "its flavour profile."),

    wellPanel(
      fluidRow(
        column(4, selectInput(ns("country"), "Country", choices = "All countries")),
        column(8, sliderInput(ns("years"), "Harvest year range",
                              min = YEAR_RANGE[1], max = YEAR_RANGE[2],
                              value = YEAR_RANGE, step = 1, sep = "", width = "100%"))
      )
    ),

    # ── Breakdown pie + what-to-show selector ───────────────────────────────────
    fluidRow(
      column(3,
        h4("Breakdown"),
        radioButtons(ns("breakdown"), NULL,
                     choices = c("Processing methods" = "method",
                                 "Regions"             = "region",
                                 "Producers"           = "producer"),
                     selected = "method"),
        p(style = "color:#7B4F2E; font-size:12px;",
          "Regions and producers show the top 5; the rest are grouped as “Other”.")),
      column(9, plotOutput(ns("pie"), height = "360px"))
    ),

    hr(),

    # ── Score distribution + altitude ───────────────────────────────────────────
    fluidRow(
      column(6, h4("Score distribution"), plotOutput(ns("hist"),   height = "320px")),
      column(6, h4("Altitude"),           plotOutput(ns("altBox"), height = "320px"))
    ),

    hr(),

    # ── Flavour profile radar ───────────────────────────────────────────────────
    fluidRow(
      column(6, offset = 3,
        h4("Flavour profile"),
        plotOutput(ns("radar"), height = "380px"))
    )
  )
}

profileServer <- function(id, data, nav = NULL) {
  moduleServer(id, function(input, output, session) {

    countries <- sort(unique(data$Country.of.Origin[data$Country.of.Origin != ""]))
    updateSelectInput(session, "country", choices = c("All countries", countries))

    # Respond to a country picked on the Global tab.
    if (!is.null(nav)) {
      observeEvent(nav$nonce, {
        req(nav$country, nav$country %in% countries)
        updateSelectInput(session, "country", selected = nav$country)
      }, ignoreInit = TRUE)
    }

    # ── Selected slice (country + year range) ───────────────────────────────────
    flt <- reactive({
      d <- data
      if (!is.null(input$country) && input$country != "All countries")
        d <- d[d$Country.of.Origin == input$country, ]
      d[!is.na(d$harvest_year) &
        d$harvest_year >= input$years[1] & d$harvest_year <= input$years[2], ]
    })
    scored_slice <- reactive({
      s <- flt(); s[!is.na(s$Total.Cup.Points) & s$Total.Cup.Points > 0, ]
    })
    slice_label <- reactive({
      c_lab <- if (is.null(input$country) || input$country == "All countries")
        "All countries" else input$country
      sprintf("%s (%d–%d)", c_lab, input$years[1], input$years[2])
    })

    # ── Breakdown pie (method / region / producer) ──────────────────────────────
    output$pie <- renderPlot({
      d <- flt()
      switch(input$breakdown,
        method   = count_pie(d$Processing.Method, 100),  # few methods: show all
        region   = count_pie(d$Region, 5),
        producer = count_pie(d$Producer, 5),
        count_pie(d$Processing.Method, 100))
    })

    # ── Score distribution ───────────────────────────────────────────────────────
    output$hist <- renderPlot({
      d <- scored_slice()
      if (nrow(d) == 0) return(gg_no_data())
      ggplot(d, aes(Total.Cup.Points)) +
        geom_histogram(binwidth = 1, fill = COFFEE_COLS$blue, colour = "white") +
        labs(x = "Total Cup Points", y = "Coffees") + theme_coffee()
    })

    # ── Altitude boxplot (continuous on the y axis) ──────────────────────────────
    output$altBox <- renderPlot({
      d <- flt()
      d <- d[!is.na(d$altitude_mean_meters) & d$altitude_mean_meters > 0 &
             d$altitude_mean_meters < 4000, ]
      if (nrow(d) == 0) return(gg_no_data("No altitude data for this selection."))
      ggplot(d, aes(x = "", y = altitude_mean_meters)) +
        geom_boxplot(fill = COFFEE_COLS$green, alpha = 0.85, width = 0.4,
                     outlier.size = 0.7, outlier.alpha = 0.4, linewidth = 0.4) +
        labs(x = NULL, y = "Altitude (m)") + theme_coffee()
    })

    # ── Flavour radar ────────────────────────────────────────────────────────────
    output$radar <- renderPlot({
      s <- scored_slice()
      if (nrow(s) == 0) {
        plot.new(); text(0.5, 0.5, "No data for this selection.", col = "#888888", cex = 1.1)
        return()
      }
      draw_radar(profile_means(s), slice_label())
    })
  })
}
