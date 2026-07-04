# Profile tab — pop-out flavour donuts
# ------------------------------------
# Pick an origin (or keep the whole world) and taste it through two interactive
# donuts. Click any slice and it pops forward while the rest of the ring fades
# back, and a callout appears with the exact percentage plus friendly ratings
# for Aroma, Flavour and Craft (the grader's own overall mark).
#   * The flavour wheel  — how the ten scored parts build the total score
#   * The craft ring     — how the beans are prepared (processing methods)
#
# Reacts to the shared `nav` bus (see server.R): clicking a country on the
# Global tab opens this tab with that origin loaded.

library(bslib)
library(plotly)

# Friendly word for a 0-10 rating.
rating_word <- function(v) {
  if (is.na(v))  return("No data")
  if (v >= 8.5)  "Outstanding"
  else if (v >= 8)   "Excellent"
  else if (v >= 7.5) "Really good"
  else if (v >= 7)   "Solid"
  else if (v >= 6)   "Fair"
  else               "Needs love"
}

# Fade a hex colour onto the cream background (recede effect for idle slices).
fade_hex <- function(hex, keep = 0.35) {
  rgb_m <- col2rgb(hex); bg <- as.vector(col2rgb("#F7F1E7"))
  mixed <- round(rgb_m * keep + bg * (1 - keep))
  rgb(mixed[1, ], mixed[2, ], mixed[3, ], maxColorValue = 255)
}

# One mini rating chip (name, value out of 10, friendly word).
mini_rating <- function(name, val) {
  div(class = "mini-rating",
      div(class = "mr-name", name),
      div(class = "mr-val", ifelse(is.na(val), "—", sprintf("%.1f", val))),
      div(class = "mr-word", rating_word(val)))
}

# Shared donut builder: stable colour per slice, pop the selected one forward
# and let the rest recede. `sel` is the selected slice index (0-based) or NULL.
tone_donut <- function(labels, values, colors, sel, source_id) {
  n    <- length(labels)
  pull <- rep(0, n)
  cols <- colors
  if (!is.null(sel) && sel >= 0 && sel < n) {
    pull[sel + 1] <- 0.16
    cols <- fade_hex(colors)
    cols[sel + 1] <- colors[sel + 1]
  }
  plot_ly(source = source_id,
          labels = labels, values = values, type = "pie", hole = 0.48,
          sort = FALSE, direction = "clockwise", rotation = -90,
          pull = pull,
          marker = list(colors = cols, line = list(color = "#F7F1E7", width = 2)),
          textinfo = "percent", insidetextorientation = "horizontal",
          textfont = list(color = "#FFFFFF", size = 12),
          hovertemplate = "<b>%{label}</b><br>%{percent}<extra>click to pop out</extra>") |>
    layout(paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
           margin = list(l = 10, r = 10, t = 10, b = 10),
           legend = list(orientation = "v", font = list(color = "#2B2018", size = 12)),
           hoverlabel = list(bgcolor = "#FFFFFF", bordercolor = "#C68642",
                             font = list(color = "#2B2018"))) |>
    config(displayModeBar = FALSE) |>
    event_register("plotly_click")
}

toneUI <- function(id) {
  ns <- NS(id)
  tagList(
    h2("Profile"),
    p(style = "max-width:860px; font-size:17px; color:#444; line-height:1.6;",
      "Every origin has a personality. Choose one below and taste it through two ",
      "rings: what its flavour is made of, and how its beans are prepared. ",
      strong("Click any slice"), " and it pops forward while the rest fade back, ",
      "revealing the exact numbers behind it."),

    wellPanel(
      fluidRow(
        column(6, selectInput(ns("country"), "Choose an origin",
                              choices = "All origins", selected = "All origins")),
        column(6, uiOutput(ns("origin_line")))
      )
    ),

    layout_columns(
      col_widths = c(6, 6),
      card(card_header("The flavour wheel: what builds the score"),
           card_body(
             plotlyOutput(ns("flav_donut"), height = "360px"),
             uiOutput(ns("flav_callout")),
             p(class = "card-note", style = "margin-top:10px;",
               "Each slice is one of the ten scored parts of the total. ",
               "Bigger slices carry more of the final mark."))),
      card(card_header("The craft ring: how the beans are prepared"),
           card_body(
             plotlyOutput(ns("craft_donut"), height = "360px"),
             uiOutput(ns("craft_callout")),
             p(class = "card-note", style = "margin-top:10px;",
               "Each slice is a processing method. Pop one out to see how its ",
               "coffees rate on aroma, flavour and craft.")))
    ),

    hr(),
    p(style = "font-size:17px; color:#2B2018; max-width:860px;",
      "The ", strong("Attributing Factors"), " tab turns from an origin's ",
      "profile to the growing conditions that shape its scores.")
  )
}

toneServer <- function(id, data, nav) {
  moduleServer(id, function(input, output, session) {

    scored <- data[!is.na(data$Total.Cup.Points) & data$Total.Cup.Points > 0, ]
    countries <- sort(unique(scored$Country.of.Origin[scored$Country.of.Origin != ""]))
    updateSelectInput(session, "country",
                      choices = c("All origins", countries), selected = "All origins")

    # Arriving from the Global tab with a country preselected.
    observeEvent(nav$nonce, {
      req(nav$country)
      if (nav$country %in% countries)
        updateSelectInput(session, "country", selected = nav$country)
    }, ignoreInit = TRUE)

    # The coffees behind the current origin choice.
    pick <- reactive({
      if (is.null(input$country) || input$country == "All origins") scored
      else scored[scored$Country.of.Origin == input$country, ]
    })
    origin_name <- reactive(
      if (is.null(input$country) || input$country == "All origins")
        "the whole world" else input$country)

    output$origin_line <- renderUI({
      d <- pick()
      div(style = "padding-top:30px; font-size:15px; color:#6F5C49;",
          sprintf("%d graded coffees, averaging %.1f points out of 100.",
                  nrow(d), mean(d$Total.Cup.Points)))
    })

    # ── The flavour wheel: ten score components ───────────────────────────────
    COMPS  <- c(FLAVOR_ATTRS, "Cupper.Points")
    comp_lab <- function(x) ifelse(x == "Cupper.Points", "Grader craft",
                                   gsub("\\.", " ", x))
    flav_df <- reactive({
      d <- pick()
      vals <- vapply(COMPS, function(a) mean(d[[a]], na.rm = TRUE), numeric(1))
      data.frame(comp = comp_lab(COMPS), val = vals,
                 share = vals / sum(vals) * 100,
                 col = cat_cols(length(COMPS)), stringsAsFactors = FALSE)
    })

    sel_flav  <- reactiveVal(NULL)   # 0-based slice index, or NULL
    sel_craft <- reactiveVal(NULL)
    observeEvent(input$country, { sel_flav(NULL); sel_craft(NULL) })

    observeEvent(event_data("plotly_click", source = "tone_flav"), {
      i <- event_data("plotly_click", source = "tone_flav")$pointNumber
      sel_flav(if (identical(sel_flav(), i)) NULL else i)
    })
    observeEvent(event_data("plotly_click", source = "tone_craft"), {
      i <- event_data("plotly_click", source = "tone_craft")$pointNumber
      sel_craft(if (identical(sel_craft(), i)) NULL else i)
    })

    output$flav_donut <- renderPlotly({
      f <- flav_df()
      tone_donut(f$comp, f$val, f$col, sel_flav(), "tone_flav")
    })

    trio <- function(d) list(aroma = mean(d$Aroma, na.rm = TRUE),
                             flav  = mean(d$Flavor, na.rm = TRUE),
                             craft = mean(d$Cupper.Points, na.rm = TRUE))

    output$flav_callout <- renderUI({
      f <- flav_df(); i <- sel_flav()
      if (is.null(i) || i < 0 || i >= nrow(f))
        return(p(class = "card-note", style = "margin-top:10px;",
                 "Nothing popped out yet. Click a slice to open it up."))
      row <- f[i + 1, ]; t <- trio(pick())
      div(class = "slice-callout", style = "margin-top:12px;",
        div(style = "display:flex; align-items:baseline; gap:14px; flex-wrap:wrap;",
            span(class = "slice-pct", sprintf("%.1f%%", row$share)),
            span(style = "font-size:16px; color:#2B2018;",
                 sprintf("of %s's total score comes from %s, rated %.1f out of 10 (%s).",
                         origin_name(), tolower(row$comp), row$val,
                         tolower(rating_word(row$val))))),
        div(style = "margin-top:6px;",
            mini_rating("Aroma", t$aroma),
            mini_rating("Flavour", t$flav),
            mini_rating("Craft", t$craft)))
    })

    # ── The craft ring: processing methods ────────────────────────────────────
    craft_df <- reactive({
      d <- pick(); d <- d[d$Processing.Method != "" & !is.na(d$Processing.Method), ]
      if (nrow(d) == 0) return(NULL)
      tab <- sort(table(d$Processing.Method), decreasing = TRUE)
      data.frame(method = names(tab), n = as.integer(tab),
                 share = as.integer(tab) / sum(tab) * 100,
                 col = cat_cols(length(tab)), stringsAsFactors = FALSE)
    })

    output$craft_donut <- renderPlotly({
      cf <- craft_df()
      validate(need(!is.null(cf), "No processing information for this origin."))
      tone_donut(cf$method, cf$n, cf$col, sel_craft(), "tone_craft")
    })

    output$craft_callout <- renderUI({
      cf <- craft_df(); i <- sel_craft()
      if (is.null(cf) || is.null(i) || i < 0 || i >= nrow(cf))
        return(p(class = "card-note", style = "margin-top:10px;",
                 "Nothing popped out yet. Click a slice to open it up."))
      row <- cf[i + 1, ]
      d <- pick(); d <- d[d$Processing.Method == row$method, ]; t <- trio(d)
      div(class = "slice-callout", style = "margin-top:12px;",
        div(style = "display:flex; align-items:baseline; gap:14px; flex-wrap:wrap;",
            span(class = "slice-pct", sprintf("%.1f%%", row$share)),
            span(style = "font-size:16px; color:#2B2018;",
                 sprintf("of %s's coffees (%d of them) are prepared %s style.",
                         origin_name(), row$n, tolower(row$method)))),
        div(style = "margin-top:6px;",
            mini_rating("Aroma", t$aroma),
            mini_rating("Flavour", t$flav),
            mini_rating("Craft", t$craft)))
    })
  })
}
