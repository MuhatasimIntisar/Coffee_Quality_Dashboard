# Global tab — the 3D globe
# --------------------------
# A spinning, draggable 3D planet (plotly orthographic choropleth) shaded by
# the chosen quality measure. It rotates gently on its own, stops the moment
# the cursor hovers a country (so the tooltip is easy to read), and zooms with
# the mouse wheel. The tooltip stays deliberately clean: the score and the
# region, nothing more. Clicking a country (globe or table) opens the Profile
# tab with that origin loaded. A ranking table, headline numbers and a
# trust-the-evidence bubble chart complete the page.
#
# Cross-tab navigation uses the shared `nav` reactiveValues (see server.R).

library(DT)
library(bslib)
library(plotly)
library(htmlwidgets)

# Dataset country names that plotly's "country names" matcher spells differently.
GLOBE_RENAME <- c(
  "Tanzania, United Republic Of" = "Tanzania",
  "Cote dIvoire"                 = "Ivory Coast",
  "United States (Puerto Rico)"  = "Puerto Rico")

# Warm sequential colorscale (cream -> espresso), same ramp as warm_fill().
GLOBE_SCALE <- list(list(0, "#F3E7D3"), list(0.25, "#E8C99A"), list(0.5, "#C68642"),
                    list(0.75, "#9C5A20"), list(1, "#3A2417"))

# Auto-rotation: spin gently, and STOP the instant the cursor hovers a country
# so the tooltip can be read in comfort. Resume once the cursor leaves. Drags
# and wheel zooms also grant a short rest.
GLOBE_SPIN_JS <- "
function(el, x){
  if (window.matchMedia && window.matchMedia('(prefers-reduced-motion: reduce)').matches) return;
  var hovering = false, pausedUntil = 0;
  function rest(ms){ pausedUntil = Date.now() + ms; }
  el.on('plotly_hover',   function(){ hovering = true;  });
  el.on('plotly_unhover', function(){ hovering = false; });
  el.addEventListener('mousedown', function(){ rest(5000); });
  el.addEventListener('wheel',     function(){ rest(5000); });
  setInterval(function(){
    if (hovering || Date.now() < pausedUntil) return;
    if (!el.layout || !el.layout.geo || !document.body.contains(el)) return;
    var rot = (el.layout.geo.projection || {}).rotation || {lon: 0};
    Plotly.relayout(el, {'geo.projection.rotation.lon': (((rot.lon || 0) + 0.35) + 180) % 360 - 180});
  }, 50);
}"

# Build the onclick that asks Shiny to navigate to a country's profile.
go_onclick <- function(input_id, country) {
  sprintf("Shiny.setInputValue(\"%s\", \"%s\", {priority:\"event\"}); return false;",
          input_id, country)
}

locationUI <- function(id) {
  ns <- NS(id)
  tagList(
    h2("Where your coffee comes from"),
    p(style = "max-width:860px; font-size:17px; color:#444; line-height:1.6;",
      "Give the planet a spin. Drag it to explore, scroll to zoom, and hover over ",
      "any shaded country to see its score and its best known region. The globe ",
      "politely stops turning while you read. Darker roast tones mean higher ",
      "scores on the measure you pick below, and clicking a country takes you ",
      "straight to its full profile."),

    wellPanel(
      fluidRow(
        column(6,
          selectInput(ns("metric"), "Colour the globe by",
                      choices = MEASURE_CHOICES, selected = "Total.Cup.Points")),
        column(6,
          sliderInput(ns("bags"), "Harvest size (bags produced)",
                      min = 0, max = 1000, value = c(0, 1000), step = 10,
                      width = "100%"))
      )
    ),

    # Globe on the left, ranking table on the right.
    layout_columns(
      col_widths = c(8, 4),
      card(card_header("A world of coffee, in 3D"),
           card_body(
             plotlyOutput(ns("globe"), height = "560px"),
             p(class = "card-note",
               "Drag to rotate. Scroll to zoom. Hover a country for its story, ",
               "click it to open its full profile."))),
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
      "Each country is plotted by its score and by how many coffees back that ",
      "score up. The further right a country sits, the more evidence it rests on. ",
      "A high score on the far left comes from just a handful of coffees, so ",
      "enjoy it with a pinch of salt."),
    plotOutput(ns("bubble"), height = "420px"),

    hr(),

    p(style = "font-size:17px; color:#2B2018; max-width:860px;",
      "Found an origin you like the look of? ", strong("Click its name"),
      " on the globe or in the table, or head to the ", strong("Profile"),
      " tab, to taste it up close.")
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

    # Per-country mean of the chosen measure, plus the story fields the globe
    # tooltip needs (partner, leading producer, best-known region). Shared by
    # the globe, the ranking table and the reliability bubble so they all agree.
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

    # A country was clicked (ranking table or globe) -> open its Profile.
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

    # ── The 3D globe: orthographic choropleth, auto-spin, drag + scroll zoom ────
    output$globe <- renderPlotly({
      cm <- country_metric()
      validate(need(!is.null(cm), "No coffees match this harvest range."))
      cm$geo_name <- ifelse(cm$country %in% names(GLOBE_RENAME),
                            GLOBE_RENAME[cm$country], cm$country)
      # Keep the tooltip clean and readable: the score and the region, plus how
      # many coffees stand behind them. No partner or producer clutter.
      cm$tip <- sprintf(
        paste0("<b>%s</b><br>",
               "%s: <b>%.1f</b><br>",
               "Region: %s<br>",
               "Coffees graded: %d"),
        cm$country, measure_label(input$metric), cm$value,
        tools::toTitleCase(cm$region), cm$n)

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
          geo = list(
            projection    = list(type = "orthographic",
                                 rotation = list(lon = -60, lat = 12)),
            showland      = TRUE,  landcolor    = "#EFE4D2",
            showocean     = TRUE,  oceancolor   = "#CBDDE8",
            showcountries = TRUE,  countrycolor = "#FFFFFF", countrywidth = 0.4,
            showcoastlines = FALSE, showframe = FALSE,
            bgcolor = "rgba(0,0,0,0)"),
          paper_bgcolor = "rgba(0,0,0,0)",
          margin = list(l = 0, r = 0, t = 0, b = 0),
          hoverlabel = list(bgcolor = "#FFFFFF", bordercolor = "#C68642",
                            font = list(color = "#2B2018", size = 13))) |>
        config(scrollZoom = TRUE, displayModeBar = FALSE) |>
        event_register("plotly_click") |>
        onRender(GLOBE_SPIN_JS)
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

    # ── Summary statistics (reflect the current bags range) ────────────────────
    output$summary_caption <- renderText(
      sprintf("Across coffees with %s to %s bags.",
              format(input$bags[1], big.mark = ","),
              format(input$bags[2], big.mark = ",")))

    kpi_h <- "120px"   # fixed height so the three cards stay even

    output$kpi_highest <- renderUI({
      cm <- country_metric()
      cm <- if (is.null(cm)) NULL else cm[cm$n >= 5, ]
      v  <- if (is.null(cm) || nrow(cm) == 0) "—" else {
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
      v <- if (nrow(d) == 0) "—" else sprintf("%.1f", mean(d[[m]], na.rm = TRUE))
      stat_card(sprintf("Average %s", tolower(measure_label(input$metric))), v,
                COFFEE_COLS$green, height = kpi_h)
    })
  })
}
