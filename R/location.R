# Global tab
# ----------
# A world view of where coffee comes from. The user picks a metric to show, a
# harvest-year range, and whether to view a leaflet bubble map or a full table.
# Either view lets the user jump to the Profile tab with a country preselected
# (table: click the country name; map: click a bubble, then "View profile" in
# the popup). Summary statistics for the current selection sit at the bottom.
#
# Cross-tab navigation uses the shared `nav` reactiveValues (see server.R):
# setting nav$country + bumping nav$nonce switches to Profile and preselects
# the country there.

library(leaflet)
library(DT)

# Metrics offered in the side dropdown (label = value). Values are columns
# produced by summarise_by() in helpers.R.
MAP_METRIC_CHOICES <- c(
  "Number of coffees"        = "n_coffees",
  "Number of producers"      = "n_producers",
  "Number of bags"           = "total_bags",
  "Coffee-producing regions" = "n_regions",
  "Average score"            = "avg_score"
)

# Format a metric value for display according to which metric it is.
fmt_metric <- function(value, metric) {
  if (length(value) == 0 || all(is.na(value))) return("—")
  if (metric == "avg_score") sprintf("%.1f", value)
  else format(round(value), big.mark = ",")
}

# Build the onclick that asks Shiny to navigate to a country's profile.
# `input_id` must be the fully-qualified (namespaced) input id.
go_onclick <- function(input_id, country) {
  sprintf("Shiny.setInputValue(\"%s\", \"%s\", {priority:\"event\"}); return false;",
          input_id, country)
}

locationUI <- function(id) {
  ns <- NS(id)
  tagList(
    h2("Where coffee comes from"),
    p("Explore origins on a map or as a table. Pick what to show, focus on a ",
      "harvest-year range, and click a country to open its full profile."),

    wellPanel(
      fluidRow(
        column(3,
          radioButtons(ns("view"), "View",
                       choices = c("Map" = "map", "Table" = "table"),
                       selected = "map", inline = TRUE)),
        column(4,
          selectInput(ns("metric"), "Show on map",
                      choices = MAP_METRIC_CHOICES, selected = "avg_score")),
        column(5,
          sliderInput(ns("years"), "Harvest year range",
                      min = YEAR_RANGE[1], max = YEAR_RANGE[2],
                      value = YEAR_RANGE, step = 1, sep = "", width = "100%"))
      )
    ),

    # Map or table, switched by the view toggle.
    conditionalPanel(
      condition = sprintf("input['%s'] == 'map'", ns("view")),
      leafletOutput(ns("map"), height = "520px")
    ),
    conditionalPanel(
      condition = sprintf("input['%s'] == 'table'", ns("view")),
      p(style = "color:#7B4F2E; font-size:13px;",
        "Click a country name to open its profile."),
      DTOutput(ns("table"))
    ),

    hr(),

    h4("Summary statistics"),
    p(textOutput(ns("summary_caption"), inline = TRUE)),
    fluidRow(
      column(3, uiOutput(ns("kpi_countries"))),
      column(3, uiOutput(ns("kpi_coffees"))),
      column(3, uiOutput(ns("kpi_producers"))),
      column(3, uiOutput(ns("kpi_bags")))
    ),
    fluidRow(
      column(3, uiOutput(ns("kpi_regions"))),
      column(3, uiOutput(ns("kpi_avgscore"))),
      column(3, uiOutput(ns("kpi_top"))),
      column(3, uiOutput(ns("kpi_bottom")))
    )
  )
}

locationServer <- function(id, data, nav) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Rows within the selected year range (drop unparseable years).
    in_range <- reactive({
      d <- data[!is.na(data$harvest_year), ]
      d[d$harvest_year >= input$years[1] & d$harvest_year <= input$years[2], ]
    })

    # One row per country with all metrics + map coordinates.
    by_country <- reactive({
      agg <- summarise_by(in_range(), "Country.of.Origin")
      merge(agg, COUNTRY_COORDS, by.x = "group", by.y = "country", all.x = TRUE)
    })

    metric_label <- reactive(
      names(MAP_METRIC_CHOICES)[MAP_METRIC_CHOICES == input$metric])

    # ── Navigate to a country's profile (table link or map popup link) ──────────
    observeEvent(input$go_country, {
      nav$country <- input$go_country
      nav$nonce   <- nav$nonce + 1
    })

    # ── Bubble map ──────────────────────────────────────────────────────────────
    output$map <- renderLeaflet({
      leaflet(options = leafletOptions(minZoom = 1, worldCopyJump = TRUE)) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        setView(lng = 10, lat = 15, zoom = 2)
    })

    # Redraw bubbles whenever the metric or year range changes (via leafletProxy
    # so the base tiles/zoom don't reset).
    observe({
      req(input$metric %in% MAP_METRIC_CHOICES)
      agg <- by_country()
      agg <- agg[!is.na(agg$lat) & !is.na(agg[[input$metric]]), ]
      proxy <- leafletProxy("map", session) %>% clearMarkers() %>% clearControls()
      if (nrow(agg) == 0) return()

      v   <- agg[[input$metric]]
      pal <- colorNumeric("YlOrBr", domain = v)
      rad <- 6 + 22 * sqrt(v / max(v))

      # Hover label: quick stats. Click popup: stats + a profile link.
      lab <- sprintf("<b>%s</b><br>%s: %s<br>Coffees: %d",
                     agg$group, metric_label(), fmt_metric(v, input$metric),
                     agg$n_coffees)
      pop <- sprintf(
        paste0("<div style='font-size:13px;'><b>%s</b><br>%s: %s<br>",
               "Coffees: %d &middot; Producers: %d<br>",
               "<a href='#' onclick='%s' ",
               "style='color:#7B4F2E; font-weight:600;'>View profile &rarr;</a></div>"),
        agg$group, metric_label(), fmt_metric(v, input$metric),
        agg$n_coffees, agg$n_producers,
        go_onclick(ns("go_country"), agg$group))

      proxy %>%
        addCircleMarkers(
          lng = agg$lon, lat = agg$lat, layerId = agg$group,
          radius = rad, color = "white", weight = 1,
          fillColor = pal(v), fillOpacity = 0.85,
          label = lapply(lab, htmltools::HTML),
          popup = pop) %>%
        addLegend("bottomright", pal = pal, values = v,
                  title = metric_label(), opacity = 0.9)
    })

    # ── Table view (everything at a glance, country names are profile links) ────
    output$table <- renderDT({
      a <- by_country()
      a <- a[order(-a$n_coffees), ]
      links <- sprintf(
        "<a href='#' onclick='%s' style='color:#7B4F2E; font-weight:600;'>%s</a>",
        vapply(a$group, function(g) go_onclick(ns("go_country"), g), character(1)),
        a$group)
      tab <- data.frame(
        Country     = links,
        Coffees     = a$n_coffees,
        Producers   = a$n_producers,
        Bags        = a$total_bags,
        Regions     = a$n_regions,
        `Avg score` = round(a$avg_score, 1),
        check.names = FALSE, stringsAsFactors = FALSE)
      datatable(tab, rownames = FALSE, escape = FALSE,
                options = list(pageLength = 15, order = list()),
                class = "stripe hover compact") %>%
        formatCurrency("Bags", currency = "", digits = 0)
    })

    # ── Summary statistics (reflect the current year range) ─────────────────────
    output$summary_caption <- renderText(
      sprintf("Across harvest years %d–%d.", input$years[1], input$years[2]))

    scored_range <- reactive({
      d <- in_range(); d[!is.na(d$Total.Cup.Points) & d$Total.Cup.Points > 0, ]
    })

    output$kpi_countries <- renderUI(
      stat_card("Countries", nrow(by_country()), COFFEE_COLS$blue))
    output$kpi_coffees <- renderUI(
      stat_card("Coffees", format(nrow(in_range()), big.mark = ","),
                COFFEE_COLS$green))
    output$kpi_producers <- renderUI(
      stat_card("Producers",
                format(length(unique(in_range()$Producer[in_range()$Producer != ""])),
                       big.mark = ","), COFFEE_COLS$orange))
    output$kpi_bags <- renderUI(
      stat_card("Total bags",
                format(sum(in_range()$Number.of.Bags, na.rm = TRUE), big.mark = ","),
                COFFEE_COLS$purple))

    output$kpi_regions <- renderUI({
      d <- in_range()
      pairs <- unique(paste(d$Country.of.Origin, d$Region)[d$Region != ""])
      stat_card("Regions", format(length(pairs), big.mark = ","), COFFEE_COLS$blue)
    })
    output$kpi_avgscore <- renderUI({
      s <- scored_range()
      v <- if (nrow(s) == 0) "—" else sprintf("%.1f", mean(s$Total.Cup.Points, na.rm = TRUE))
      stat_card("Average score", v, COFFEE_COLS$green)
    })
    output$kpi_top <- renderUI({
      ca <- by_country(); ca <- ca[ca$n_coffees >= 5 & !is.na(ca$avg_score), ]
      v <- if (nrow(ca) == 0) "—" else
        paste0(ca$group[which.max(ca$avg_score)], " — ",
               sprintf("%.1f", max(ca$avg_score)))
      stat_card("Highest avg score (≥5 coffees)", v, COFFEE_COLS$orange)
    })
    output$kpi_bottom <- renderUI({
      ca <- by_country(); ca <- ca[ca$n_coffees >= 5 & !is.na(ca$avg_score), ]
      v <- if (nrow(ca) == 0) "—" else
        paste0(ca$group[which.min(ca$avg_score)], " — ",
               sprintf("%.1f", min(ca$avg_score)))
      stat_card("Lowest avg score (≥5 coffees)", v, COFFEE_COLS$purple)
    })
  })
}
