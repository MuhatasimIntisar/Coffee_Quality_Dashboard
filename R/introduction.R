# Overview tab — question, evidence, method
# ------------------------------------------
# Content carried over from the 3-July (Intisar) Overview, re-presented in a
# cleaner layout. Same flow, top to bottom:
#   1. What this dashboard is for — the framing question, with the tab
#      walkthrough shown as a clickable "journey" strip instead of a sentence.
#   2. Why you can trust the findings — the five headline evidence counts.
#   3. How coffee is rated — the 0–100 scale, the average scorecard ring, and
#      a plain-language glossary of the nine flavour attributes (now a grid).
#   4. A closing call-to-action into the Global tab.
#
# All styles for this page live in the tags$style block below (prefixed .ov-),
# so nothing outside this file needs to change.

library(bslib)

# Plain-language meaning of each of the nine scored flavour attributes.
ATTR_DESC <- c(
  Aroma      = "How the coffee smells, from the dry grounds to the brewed cup.",
  Flavor     = "The overall taste character; the coffee's main impression on the palate.",
  Aftertaste = "The flavour that lingers once the coffee has been swallowed.",
  Acidity    = "Brightness or liveliness. A pleasant tang, not sourness.",
  Body       = "The weight and texture of the coffee in the mouth.",
  Balance    = "How well flavour, acidity, body and aftertaste work together.",
  Uniformity = "Consistency of flavour from cup to cup of the same coffee.",
  Clean.Cup  = "Freedom from off flavours or defects. A clean taste.",
  Sweetness  = "Natural sweetness, free from sour or harsh notes."
)

# The reading journey through the dashboard (was a prose sentence in the
# 3-July version: "where... what... origin... conclusions"). Each step is a
# clickable chip that opens its tab via the existing top-level `go_tab` input.
OV_STEPS <- list(
  list(tab = "Global",              word = "Where",       desc = "the geographic distribution of quality"),
  list(tab = "Profile",             word = "Origin",      desc = "a detailed profile of a single origin"),
  list(tab = "Attributing Factors", word = "What",        desc = "growing and processing factors associated with quality"),
  list(tab = "Sensory Analysis",    word = "How",         desc = "the nine sensory attributes compare across origins"),
  list(tab = "Conclusion",          word = "Conclusions", desc = "the principal findings and conclusions")
)

introductionUI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$style(HTML("
      /* ── Overview page styles (self-contained) ─────────────────────── */
      .ov-hero{ margin:6px 0 4px; max-width:900px; }
      .ov-hero h2{ font-size:32px; line-height:1.15; margin-bottom:12px; }
      .ov-lead{ font-size:17px; color:#4A3B2C; line-height:1.65; max-width:820px; }

      /* Journey strip: five clickable steps */
      .ov-steps{ display:grid; grid-template-columns:repeat(auto-fit, minmax(170px, 1fr));
                 gap:12px; margin:20px 0 8px; }
      .ov-step{ display:flex; flex-direction:column; background:#FFFFFF; border:1px solid #EADDCB;
                border-radius:14px; padding:14px 16px 12px;
                position:relative; overflow:hidden;
                box-shadow:0 1px 2px rgba(58,36,23,.05); }
      .ov-step::before{ content:''; position:absolute; inset:0 auto 0 0; width:4px;
                        background:var(--ov-accent,#9C5A20); }
      .ov-step-num{ font-family:system-ui,-apple-system,'Segoe UI',Roboto,Helvetica,Arial,sans-serif; font-size:10.5px;
                    letter-spacing:.14em; text-transform:uppercase;
                    color:var(--ov-accent,#9C5A20); margin-bottom:6px; }
      .ov-step-word{ font-size:19px; font-weight:700; color:#2B2018; line-height:1.1;
                     margin-bottom:4px; }
      .ov-step-desc{ font-size:13px; color:#6F5C49; line-height:1.45; }
      .ov-step-link{ align-self:flex-start; margin-top:auto; padding-top:12px;
                     font-size:13.5px; font-weight:600; color:var(--ov-accent,#9C5A20);
                     text-decoration:none; }
      .ov-step-link:hover{ text-decoration:underline; }

      /* Section kicker + heading */
      .ov-kicker{ font-family:system-ui,-apple-system,'Segoe UI',Roboto,Helvetica,Arial,sans-serif; font-size:11.5px;
                  letter-spacing:.18em; text-transform:uppercase; color:#9C5A20;
                  margin:0 0 6px; }
      .ov-sec{ margin-top:6px; }
      .ov-sec h4{ margin-bottom:8px; }
      .ov-note{ font-size:15px; color:#4A3B2C; line-height:1.6; max-width:820px;
                margin-bottom:16px; }

      /* Glossary grid: nine small definition cards */
      .ov-gloss{ display:grid; grid-template-columns:repeat(auto-fit, minmax(240px, 1fr));
                 gap:12px; margin-top:14px; max-width:1100px; }
      .ov-term{ background:#FFFFFF; border:1px solid #EADDCB; border-radius:12px;
                padding:12px 14px; box-shadow:0 1px 2px rgba(58,36,23,.04); }
      .ov-term-name{ display:inline-block; border-radius:6px; padding:2px 10px;
                     font-size:12.5px; font-weight:700; margin-bottom:7px; }
      .ov-term-desc{ font-size:13.5px; color:#4A3B2C; line-height:1.5; margin:0; }

      /* Closing call-to-action */
      .ov-cta{ display:inline-flex; align-items:center; gap:10px; margin:4px 0 10px;
               background:linear-gradient(120deg,#9C5A20,#C68642); color:#fff;
               font-size:16px; font-weight:600; padding:12px 22px; border-radius:12px;
               text-decoration:none; box-shadow:0 5px 16px rgba(156,90,32,.3);
               transition:box-shadow .2s ease, filter .2s ease; }
      .ov-cta:hover{ color:#fff; text-decoration:none; filter:brightness(1.05);
                     box-shadow:0 8px 22px rgba(156,90,32,.4); }
    ")),

    # ── 1 · The framing question + clickable journey ─────────────────────────
    div(class = "ov-hero",
        div(class = "ov-kicker", "Data source · Coffee Quality Institute"),
        h2("Introduction"),
        h5("What this dashboard is for."),
        p(class = "ov-lead",
          "This dashboard examines the factors that distinguish exceptional ",
          "coffee from ordinary coffee, and identifies the regions in which the ",
          "highest-quality beans are produced, using independent quality ",
          "gradings from the Coffee Quality Institute.")),

    hr(),

    # ── 2 · Why you can trust these findings ─────────────────────────────────
    div(class = "ov-sec",
        div(class = "ov-kicker", "The evidence"),
        h4("Dataset Overview"),
        h5("What the data looks like."),
        p(class = "ov-note",
          "All findings are derived from a large, independent body of ",
          "professional cup scores. For each coffee the dataset also records its ",
          "growing conditions — altitude, bean moisture and processing method — ",
          "alongside its origin and harvest year:")),
    layout_column_wrap(
      width = 1/4,
      uiOutput(ns("kpi_coffees")),
      uiOutput(ns("kpi_countries")),
      uiOutput(ns("kpi_regions")),
      uiOutput(ns("kpi_producers")),
      uiOutput(ns("kpi_partners")),
      uiOutput(ns("kpi_years")),
      uiOutput(ns("kpi_methods"))
    ),

    hr(),

    # ── 3 · How coffee is rated ───────────────────────────────────────────────
    div(class = "ov-sec",
        div(class = "ov-kicker", "The method"),
        h4("Scoring methodology"),
        p(class = "ov-note",
          "Each coffee in the dataset has been evaluated by certified graders ",
          "and assigned a Total Cup Points score ", strong("out of 100"),
          ", comprising ten components each scored out of 10: nine distinct ",
          "sensory attributes (defined below) and the grader's overall ",
          "assessment. ",
          textOutput(ns("scale_note"), inline = TRUE))),
    scorecard_card(plotOutput(ns("scorecard_ring"), height = "300px"),
                   uiOutput(ns("scorecard_legend")), n = N_SCORED),

    uiOutput(ns("glossary")),

    hr(),

    # ── 4 · Navigation: the analysis in five steps ────────────────────────────
    div(class = "ov-sec",
        div(class = "ov-kicker", "Navigation"),
        h4("Explore the analysis"),
        p(class = "ov-note",
          "Each stage of the analysis can be opened directly from the cards ",
          "below, or from the tabs at the top of the page.")),
    uiOutput(ns("steps"))
  )
}

introductionServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {

    nonblank <- function(x) x[!is.na(x) & x != ""]

    # ── Journey strip: one step per tab; only the bottom link opens the tab ────
    output$steps <- renderUI({
      accents <- cat_cols(length(OV_STEPS))
      div(class = "ov-steps",
        lapply(seq_along(OV_STEPS), function(i) {
          s <- OV_STEPS[[i]]
          div(class = "ov-step", style = paste0("--ov-accent:", accents[i], ";"),
              div(class = "ov-step-num", sprintf("Step %d", i)),
              div(class = "ov-step-word", s$word),
              div(class = "ov-step-desc", s$desc),
              tags$a(class = "ov-step-link", href = "#",
                     onclick = sprintf(
                       "Shiny.setInputValue('go_tab','%s',{priority:'event'}); return false;",
                       s$tab),
                     paste0(s$tab, " →")))
        }))
    })

    # ── Headline evidence counts ──────────────────────────────────────────────
    kpi_h <- "120px"   # fixed height so every KPI card stays the same size
    output$kpi_coffees <- renderUI(
      stat_card("Coffees evaluated", format(nrow(data), big.mark = ","),
                COFFEE_COLS$green, height = kpi_h))
    output$kpi_countries <- renderUI(
      stat_card("Countries", length(unique(nonblank(data$Country.of.Origin))),
                COFFEE_COLS$blue, height = kpi_h))
    output$kpi_regions <- renderUI({
      pairs <- unique(paste(data$Country.of.Origin, data$Region)[data$Region != ""])
      stat_card("Growing regions", format(length(pairs), big.mark = ","),
                COFFEE_COLS$orange, height = kpi_h)
    })
    output$kpi_producers <- renderUI(
      stat_card("Producers", format(length(unique(nonblank(data$Producer))),
                                    big.mark = ","), COFFEE_COLS$purple, height = kpi_h)
    )
    output$kpi_partners <- renderUI(
      stat_card("Grading partners",
                length(unique(nonblank(data$In.Country.Partner))),
                COFFEE_COLS$green, height = kpi_h))
    output$kpi_years <- renderUI({
      yr <- range(data$harvest_year, na.rm = TRUE)
      stat_card("Harvest years", sprintf("%d–%d", yr[1], yr[2]),
                COFFEE_COLS$blue, height = kpi_h)
    })
    output$kpi_methods <- renderUI(
      stat_card("Processing methods",
                length(unique(nonblank(data$Processing.Method))),
                COFFEE_COLS$orange, height = kpi_h))

    # ── Glossary: nine definition cards, chips coloured to match the ring ─────
    output$glossary <- renderUI({
      pal <- setNames(cat_cols(length(FLAVOR_ATTRS) + 1),
                      c(FLAVOR_ATTRS, "Cupper.Points"))
      # Dark or white text depending on how light the chip colour is.
      text_on <- function(bg) {
        r <- col2rgb(bg)
        lum <- (0.299 * r[1] + 0.587 * r[2] + 0.114 * r[3]) / 255
        if (lum > 0.6) "#2B2018" else "#FFFFFF"
      }
      div(class = "ov-gloss",
        lapply(names(ATTR_DESC), function(a)
          div(class = "ov-term",
              tags$span(class = "ov-term-name",
                        style = paste0("background:", pal[[a]], "; color:",
                                       text_on(pal[[a]]), ";"),
                        gsub("\\.", " ", a)),
              tags$p(class = "ov-term-desc", ATTR_DESC[[a]]))))
    })

    # Where scores actually land in THIS dataset (no external threshold).
    output$scale_note <- renderText({
      d <- data$Total.Cup.Points[!is.na(data$Total.Cup.Points) & data$Total.Cup.Points > 0]
      sprintf("Within this dataset, scores range from %.0f to %.0f points, with a mean of %.0f.",
              min(d), max(d), mean(d))
    })

    # Average scorecard: a ring + a progress-bar legend (shared helpers).
    sc_df <- scorecard_df(data[!is.na(data$Total.Cup.Points) & data$Total.Cup.Points > 0, ])
    output$scorecard_ring   <- renderPlot(scorecard_ring(sc_df))
    output$scorecard_legend <- renderUI(scorecard_legend(sc_df))
  })
}
