# Global tab — static world map
# ------------------------------
# A flat, static choropleth (plotly) shaded by the chosen quality measure,
# displayed inside a standard rounded dashboard card. No rotation, panning or
# automatic animation: the map holds its position. Hovering a highlighted
# country shows its score, best-known region and evidence count; clicking a
# country (map or table) opens the Profile tab with that origin loaded. A
# ranking table, headline numbers and an evidence-reliability chart complete
# the page.
#
# Cross-tab navigation uses the shared `nav` reactiveValues (see server.R).

library(DT)
library(bslib)
library(plotly)

# Dataset country names that plotly's "country names" matcher spells differently.
GLOBE_RENAME <- c(
  "Tanzania, United Republic Of" = "Tanzania",
  "Cote dIvoire"                 = "Ivory Coast",
  "United States (Puerto Rico)"  = "Puerto Rico")

# Warm sequential colorscale (cream -> espresso), same ramp as warm_fill().
GLOBE_SCALE <- list(list(0, "#F3E7D3"), list(0.25, "#E8C99A"), list(0.5, "#C68642"),
                    list(0.75, "#9C5A20"), list(1, "#3A2417"))

# Build the onclick that asks Shiny to navigate to a country's profile.
go_onclick <- function(input_id, country) {
  sprintf("Shiny.setInputValue(\"%s\", \"%s\", {priority:\"event\"}); return false;",
          input_id, country)
}

locationUI <- function(id) {
  ns <- NS(id)
  tagList(
    h2("Global Coffee Origins"),
    h5("How coffee from different countries score."),
    p(style = "max-width:860px; font-size:17px; color:#444; line-height:1.6;",
      "This view maps average quality by country of origin. Select a quality ",
      "measure below: producing countries are shaded by their average, with ",
      "darker tones indicating higher values. Hover over a highlighted country ",
      "for its score, leading region and sample size; click a country on the ",
      "map or in the table to open its full profile."),

    wellPanel(
      fluidRow(
        column(6,
          selectInput(ns("metric"), "Colour map by",
                      choices = MEASURE_CHOICES, selected = "Total.Cup.Points")),
        column(6,
          sliderInput(ns("bags"), "Harvest size (bags produced)",
                      min = 0, max = 1000, value = c(0, 1000), step = 10,
                      width = "100%"))
      )
    ),

    # Map on the left, ranking table beside it on the right.
    layout_columns(
      col_widths = c(8, 4),
      card(card_header("Global Coffee Origins"),
           card_body(
             plotlyOutput(ns("globe"), height = "600px"),
             p(class = "card-note",
               "Countries shaded by the selected measure. Hover for details; ",
               "click a country to open its profile."))),
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
    h4("Evidence behind each origin's score"),
    p(style = "color:#6F5C49; font-size:13px; max-width:820px;",
      "Each country is plotted by its average score and the number of coffees ",
      "behind that average. Countries further right rest on more evidence and ",
      "their scores are more reliable; a high score on the far left is based ",
      "on only a handful of coffees and should be interpreted with caution."),
    plotOutput(ns("bubble"), height = "420px")
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

    most_common <- function(x) {
      x <- x[!is.na(x) & x != ""]
      if (length(x) == 0) return("Not recorded")
      names(which.max(table(x)))
    }

    # Per-country mean of the chosen measure, plus the fields the map tooltip
    # needs (partner, leading producer, best-known region). Shared by the map,
    # the ranking table and the reliability chart so they all agree.
    country_metric <- reactive({
      m <- input$metric
      d <- in_range(); d <- d[d$Country.of.Origin != "" & !is.na(d[[m]]), ]
      if (m == "Total.Cup.Points") d <- d[d[[m]] > 0, ]
      if (nrow(d) == 0) return(NULL)
      parts <- split(d, d$Country.of.Origin)
      out <- lapply(names(parts), function(g) {
        p <- parts[[g]]
        data.frame(country  = g,
                   value    = mean(p[[m]], na.rm = TRUE),
                   n        = nrow(p),
                   bags     = sum(p$Number.of.Bags, na.rm = TRUE),
                   partner  = most_common(p$In.Country.Partner),
                   producer = most_common(p$Producer),
                   region   = most_common(p$Region),
                   stringsAsFactors = FALSE)
      })
      do.call(rbind, out)
    })

    # A country was clicked (ranking table or map) -> open its Profile.
    observeEvent(input$go_country, {
      nav$country <- input$go_country
      nav$nonce   <- nav$nonce + 1
    })
    observeEvent(event_data("plotly_click", source = "globe"), {
      ev <- event_data("plotly_click", source = "globe")
      req(ev$customdata)
      nav$country <- ev$customdata
      nav$nonce   <- nav$nonce + 1
    })

    # ── Static world map: flat choropleth, fixed position, hover + click ────────
    output$globe <- renderPlotly({
      cm <- country_metric()
      validate(need(!is.null(cm), "No coffees match this harvest range."))
      cm$geo_name <- ifelse(cm$country %in% names(GLOBE_RENAME),
                            GLOBE_RENAME[cm$country], cm$country)
      # Tooltip: country name, the score, the total bags produced, and how many
      # coffees stand behind the average.
      cm$tip <- sprintf(
        paste0("<b>%s</b><br>",
               "%s: <b>%.1f</b><br>",
               "Bags produced: %s<br>",
               "Coffees graded: %d"),
        cm$country, measure_label(input$metric), cm$value,
        format(cm$bags, big.mark = ",", trim = TRUE), cm$n)

      plot_ly(cm, source = "globe",
              type = "choropleth", locationmode = "country names",
              locations = ~geo_name, z = ~value,
              customdata = ~country,
              text = ~tip, hovertemplate = "%{text}<extra></extra>",
              colorscale = GLOBE_SCALE,
              marker = list(line = list(color = "#F7F1E7", width = 0.6)),
              colorbar = list(title = list(text = measure_label(input$metric),
                                           font = list(size = 12)),
                              thickness = 12, len = 0.6,
                              tickfont = list(color = "#6F5C49"))) |>
        layout(
          dragmode = FALSE,                      # no panning or rotation
          geo = list(
            projection    = list(type = "natural earth"),
            lataxis       = list(range = c(-58, 84)),   # trim empty polar space
            showland      = TRUE,  landcolor    = "#EFE4D2",
            showocean     = TRUE,  oceancolor   = "#F7F1E7",
            showcountries = TRUE,  countrycolor = "#FFFFFF", countrywidth = 0.4,
            showcoastlines = FALSE, showframe = FALSE,
            bgcolor = "rgba(0,0,0,0)"),
          paper_bgcolor = "rgba(0,0,0,0)",
          margin = list(l = 0, r = 0, t = 0, b = 0),
          hoverlabel = list(bgcolor = "#FFFFFF", bordercolor = "#C68642",
                            font = list(color = "#2B2018", size = 13))) |>
        config(scrollZoom = FALSE, displayModeBar = FALSE) |>
        event_register("plotly_click")
    })

    # ── Ranking table: Country + score (>= 5 coffees), country links onward ────
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

    # ── Reliability bubble: score (y) vs number of coffees (x), sized by bags ──
    # Light, soft colour scheme so country labels stay readable:
    #   * bubbles       — soft latte-to-light-caramel/gold fill by the selected
    #                     score measure (Total Cup Points by default)
    #   * Total bags    — light teal/green legend accent (bubble size)
    #   * No. of coffees— soft blue axis accent (x position)
    output$bubble <- renderPlot({
      cm <- country_metric()
      if (is.null(cm)) return(gg_no_data("No data for this selection."))
      ggplot(cm, aes(n, value, size = bags, fill = value)) +
        geom_point(shape = 21, colour = "#C68642", alpha = 0.9, stroke = 0.6) +
        geom_text(aes(label = country), size = 3, colour = "#3A2417", vjust = -0.9,
                  fontface = "bold", check_overlap = TRUE, show.legend = FALSE) +
        scale_fill_gradient(low = "#F6EBD3", high = "#E8B96B", guide = "none") +
        scale_size(range = c(3, 15), name = "Total bags") +
        labs(x = "Number of coffees",
             y = measure_label(input$metric)) +
        coord_cartesian(ylim = measure_limits(input$metric)) + theme_coffee() +
        theme(axis.title.y  = element_text(colour = "#A8681B", face = "bold"),
              axis.title.x  = element_text(colour = "#4E7FA0", face = "bold"),
              legend.title  = element_text(colour = "#2F9E7D", face = "bold"))
    })

    # ── Summary statistics (reflect the current bags range) ────────────────────
    output$summary_caption <- renderText(
      sprintf("Across coffees with %s to %s bags.",
              format(input$bags[1], big.mark = ","),
              format(input$bags[2], big.mark = ",")))

    kpi_h <- "120px"   # fixed height so the three cards stay even

    output$kpi_highest <- renderUI({
      cm <- country_metric()
      cm <- if (is.null(cm)) NULL else cm[cm$n >= 5, ]
      v  <- if (is.null(cm) || nrow(cm) == 0) "N/A" else {
        top <- cm[which.max(cm$value), ]
        paste0(top$country, " at ", sprintf("%.1f", top$value))
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
      v <- if (nrow(d) == 0) "N/A" else sprintf("%.1f", mean(d[[m]], na.rm = TRUE))
      stat_card(sprintf("Average %s", tolower(measure_label(input$metric))), v,
                COFFEE_COLS$green, height = kpi_h)
    })
  })
}
