# Global tab
# ----------
# A world view of where the best coffee comes from. One metric (a flavour
# attribute or the overall score) drives the choropleth map (left) and a simple
# ranking table of Country + score (right); click a country name to open its
# Profile. A number-of-bags slider scopes everything. Summary statistics sit
# below, and a reliability bubble (quality vs how many coffees back each score)
# sits at the very bottom for anyone who wants to gauge trust.
#
# Cross-tab navigation uses the shared `nav` reactiveValues (see server.R):
# setting nav$country + bumping nav$nonce switches to Profile and preselects it.
#
# The map is a ggplot2 choropleth from map_data("world"); this needs the
# lightweight `maps` package installed (install.packages("maps")). No leaflet.

library(DT)
library(bslib)

# Dataset country names that differ from map_data("world") region names.
MAP_RENAME <- c(
  "United States"                = "USA",
  "United States (Puerto Rico)"  = "Puerto Rico",
  "Tanzania, United Republic Of" = "Tanzania",
  "Cote dIvoire"                 = "Ivory Coast")

# Build the onclick that asks Shiny to navigate to a country's profile.
go_onclick <- function(input_id, country) {
  sprintf("Shiny.setInputValue(\"%s\", \"%s\", {priority:\"event\"}); return false;",
          input_id, country)
}

locationUI <- function(id) {
  ns <- NS(id)
  tagList(
    h2("Where coffee comes from"),
    p(style = "max-width:860px; font-size:17px; color:#444; line-height:1.6;",
      "This page maps ", strong("where"), " the world's coffee is grown and which ",
      "origins score best. Choose a flavour attribute or the overall score below: ",
      "the map shades each country by its average and the table ranks the top ",
      "countries — click any country to open its full profile. The bubble at the ",
      "bottom shows how much evidence backs each score."),

    wellPanel(
      fluidRow(
        column(6,
          selectInput(ns("metric"), "Colour / rank by",
                      choices = MEASURE_CHOICES, selected = "Total.Cup.Points")),
        column(6,
          sliderInput(ns("bags"), "Number of bags",
                      min = 0, max = 1000, value = c(0, 1000), step = 10,
                      width = "100%"))
      )
    ),

    # Map on the left, ranking table on the right.
    layout_columns(
      col_widths = c(8, 4),
      card(card_header("Where it's grown"),
           plotOutput(ns("map"), height = "520px")),
      card(card_header(textOutput(ns("rank_title"))),
           DTOutput(ns("rank_table")))
    ),

    hr(),

    h4("At a glance"),
    p(textOutput(ns("summary_caption"), inline = TRUE)),
    fluidRow(
      column(4, uiOutput(ns("kpi_highest"))),
      column(4, uiOutput(ns("kpi_ranked"))),
      column(4, uiOutput(ns("kpi_avg")))
    ),

    hr(),

    # Reliability lens: how much evidence backs each country's score.
    h4("How much to trust each origin"),
    p(style = "color:#6F5C49; font-size:13px; max-width:820px;",
      "Each country plotted by its score and how many coffees back it. Countries ",
      "further right rest on more coffees, so trust those scores more; a high ",
      "score far to the left sits on very few coffees — treat it with caution."),
    plotOutput(ns("bubble"), height = "420px"),

    hr(),

    p(style = "font-size:17px; color:#2B2018; max-width:860px;",
      "Spotted an interesting origin? ", strong("Click a country"), " on the map or ",
      "table above — or open the ", strong("Profile"), " tab — to dig into a single ",
      "origin in depth.")
  )
}

locationServer <- function(id, data, nav) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Set the bags slider to the data's real range once at startup.
    bmax <- max(data$Number.of.Bags, na.rm = TRUE)
    updateSliderInput(session, "bags", max = bmax, value = c(0, bmax))

    # Rows within the selected number-of-bags range.
    in_range <- reactive({
      d <- data[!is.na(data$Number.of.Bags), ]
      d[d$Number.of.Bags >= input$bags[1] & d$Number.of.Bags <= input$bags[2], ]
    })

    # Per-country mean of the chosen measure (+ coffees + total bags). Shared by
    # the map, the ranking table and the reliability bubble so they all agree.
    country_metric <- reactive({
      m <- input$metric
      d <- in_range(); d <- d[d$Country.of.Origin != "" & !is.na(d[[m]]), ]
      if (m == "Total.Cup.Points") d <- d[d[[m]] > 0, ]
      if (nrow(d) == 0) return(NULL)
      ag <- aggregate(d[[m]], list(country = d$Country.of.Origin), mean, na.rm = TRUE)
      names(ag) <- c("country", "value")
      tab      <- table(d$Country.of.Origin)
      ag$n     <- as.integer(tab[ag$country])
      bags     <- tapply(d$Number.of.Bags, d$Country.of.Origin, sum, na.rm = TRUE)
      ag$bags  <- as.numeric(bags[ag$country])
      ag
    })

    # A country was clicked in the ranking table -> open its Profile.
    observeEvent(input$go_country, {
      nav$country <- input$go_country
      nav$nonce   <- nav$nonce + 1
    })

    # ── Choropleth: whole countries filled by the chosen metric ─────────────────
    output$map <- renderPlot({
      cm <- country_metric()
      if (is.null(cm)) return(gg_no_data("No data for this selection."))
      cm$region <- ifelse(cm$country %in% names(MAP_RENAME),
                          MAP_RENAME[cm$country], cm$country)
      world <- map_data("world")
      world$value <- cm$value[match(world$region, cm$region)]
      ggplot(world, aes(long, lat, group = group)) +
        geom_polygon(aes(fill = value), colour = "white", linewidth = 0.1) +
        warm_fill(measure_label(input$metric)) +
        coord_quickmap() + labs(x = NULL, y = NULL) +
        theme_void(base_size = 12) +
        theme(legend.position = "right", plot.margin = margin(2, 2, 2, 2))
    })

    # ── Ranking table: Country + score (>= 5 coffees), country links to Profile ─
    output$rank_title <- renderText(
      sprintf("Top countries by %s", tolower(measure_label(input$metric))))
    output$rank_table <- renderDT({
      cm <- country_metric()
      if (is.null(cm)) return(datatable(data.frame(), rownames = FALSE))
      cm <- cm[cm$n >= 5, ]
      cm <- cm[order(-cm$value), ]
      links <- sprintf(
        "<a href='#' onclick='%s' style='color:#7B4F2E; font-weight:600;'>%s</a>",
        vapply(cm$country, function(g) go_onclick(ns("go_country"), g), character(1)),
        cm$country)
      tab <- data.frame(Country = links, Score = round(cm$value, 1),
                        check.names = FALSE, stringsAsFactors = FALSE)
      names(tab)[2] <- measure_label(input$metric)
      datatable(tab, rownames = FALSE, escape = FALSE,
                options = list(pageLength = 12, order = list(), dom = "tp"),
                class = "stripe hover compact")
    })

    # ── Reliability bubble: score (y) vs number of coffees (x), sized by bags ────
    output$bubble <- renderPlot({
      cm <- country_metric()
      if (is.null(cm)) return(gg_no_data("No data for this selection."))
      ggplot(cm, aes(n, value, size = bags)) +
        geom_point(colour = COFFEE_COLS$blue, alpha = 0.65) +
        geom_text(aes(label = country), size = 3, colour = LATTE$text, vjust = -0.9,
                  check_overlap = TRUE, show.legend = FALSE) +
        scale_size(range = c(3, 15), name = "Total bags") +
        labs(x = "Number of coffees (how much evidence)",
             y = measure_label(input$metric)) + theme_coffee()
    })

    # ── Summary statistics (reflect the current bags range) ─────────────────────
    output$summary_caption <- renderText(
      sprintf("Across coffees with %s–%s bags.",
              format(input$bags[1], big.mark = ","),
              format(input$bags[2], big.mark = ",")))

    kpi_h <- "120px"   # fixed height so the three cards stay even

    output$kpi_highest <- renderUI({
      cm <- country_metric()
      cm <- if (is.null(cm)) NULL else cm[cm$n >= 5, ]
      v  <- if (is.null(cm) || nrow(cm) == 0) "—" else {
        top <- cm[which.max(cm$value), ]
        paste0(top$country, " — ", sprintf("%.1f", top$value))
      }
      stat_card(sprintf("Highest %s", tolower(measure_label(input$metric))), v,
                COFFEE_COLS$orange, height = kpi_h)
    })
    output$kpi_ranked <- renderUI({
      cm <- country_metric()
      n  <- if (is.null(cm)) 0L else sum(cm$n >= 5)
      stat_card("Countries ranked", n, COFFEE_COLS$blue, height = kpi_h)
    })
    output$kpi_avg <- renderUI({
      m <- input$metric
      d <- in_range(); d <- d[!is.na(d[[m]]), ]
      if (m == "Total.Cup.Points") d <- d[d[[m]] > 0, ]
      v <- if (nrow(d) == 0) "—" else sprintf("%.1f", mean(d[[m]], na.rm = TRUE))
      stat_card(sprintf("Average %s", tolower(measure_label(input$metric))), v,
                COFFEE_COLS$green, height = kpi_h)
    })
  })
}
