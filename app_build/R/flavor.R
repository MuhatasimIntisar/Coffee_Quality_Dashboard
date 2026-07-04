# Sensory Analysis tab: Used to figure out how score
# relates to attributes
# We have 3 segments:
#   1. Feature importance — every sensory attribute ranked by how closely it
#      tracks the score (total score or the grader's overall mark).
#   2. Scatter Plot -  scatter plot for attribute vs score. With click to find 
#      overall score for a single point.
#   2. Make your own coffee — one 1-10 slider per flavor attribute and a
#      button. Find most matching coffee.

library(bslib)

# The coffee builder lets the user dial in a full flavour profile: one slider
# per scored attribute (FLAVOR_ATTRS). builder_ids() maps each attribute to
# its slider inputId

builder_ids <- function()
  setNames(paste0("p_", gsub("\\.", "_", FLAVOR_ATTRS)), FLAVOR_ATTRS)

flavorUI <- function(id) {
  ns <- NS(id)
  tagList(
    h2("Sensory Analysis"),
    p(style = "max-width:860px; font-size:17px; color:#444; line-height:1.6;",
      "Which tasting notes actually decide a coffee's score? This ranks every ",
      "sensory attribute by how closely it tracks the score — the longer the bar, ",
      "the more it drives quality. Switch between the final total score and the ",
      "grader's own overall mark; the ranking barely changes, which is how you know ",
      "the scoring is consistent. Then see how any note tracks the score, and ",
      "build your own cup."),

    # ── 1. Feature importance of the sensory attributes ─────────────────────
    card(
      card_header(textOutput(ns("imp_title"))),
      card_body(
        radioButtons(ns("target"), "Measured against",
                     choices = c("Total score"           = "Total.Cup.Points",
                                 "Grader's overall mark"  = "Cupper.Points"),
                     selected = "Total.Cup.Points", inline = TRUE),
        plotOutput(ns("importance"), height = "420px"),
        p(class = "card-note",
          "How closely each attribute tracks the chosen score. Aftertaste and ",
          "flavour lead by a distance; the near-perfect attributes (sweetness, ",
          "clean cup, uniformity) barely move it, because almost every coffee ",
          "scores top marks on those."))),

    hr(),

    # 2. scatter plot to identify how close it tracks 
    card(
      card_header("Relationship Between Attribute and Score"),
      card_body(
        radioButtons(ns("funnel_attr"), "Select Attribute",
                     choices  = setNames(FLAVOR_ATTRS, gsub("\\.", " ", FLAVOR_ATTRS)),
                     selected = "Aftertaste", inline = TRUE),
        layout_columns(
          col_widths = c(7, 5),
          plotOutput(ns("funnel"), height = "440px", click = ns("funnel_click")),
          plotOutput(ns("funnel_pick"), height = "440px")),
        div(class = "card-note",
          tags$ol(style = "margin:0; padding-left:20px;",
            tags$li("The distribution narrows towards the top: as the selected ",
                    "attribute's score rises, the spread of total scores contracts, ",
                    "indicating that higher-scoring coffees converge on a similar ",
                    "profile while lower-scoring coffees vary widely."),
            tags$li("Sweetness, uniformity and clean cup are effectively binary: ",
                    "almost every coffee is awarded full marks, so their points bank ",
                    "against the right-hand edge and the attribute distinguishes ",
                    "coffees only when a defect is present."),
            tags$li("Consequently, low total scores arise chiefly when clean cup or ",
                    "sweetness collapses, rather than from a uniform decline across ",
                    "all attributes.")),
          tags$p(style = "margin:8px 0 0;",
                 "Select any point to view that coffee's full component breakdown ",
                 "to the right.")))),

    hr(),

    # ── 3. Make your own coffee ─────────────────────────────────────────────
    h4("Select a Coffee"),
    p(style = "max-width:860px; font-size:15px; color:#444; line-height:1.6;",
      "Set a value from 1 to 10 for each part of the flavour profile, then press ",
      "the button. We compare your profile against every graded coffee here and ",
      "list the closest matches."),

    layout_columns(
      col_widths = c(4, 8),

      # Left: one plain slider per flavour-profile attribute, then the button.
      wellPanel(
        h5(style = "margin-top:0; font-weight:600;", "Your flavour profile"),
        lapply(FLAVOR_ATTRS, function(a)
          sliderInput(ns(paste0("p_", gsub("\\.", "_", a))),
                      gsub("\\.", " ", a),
                      min = 1, max = 10, value = 7, step = 0.5, ticks = FALSE)),
        actionButton(ns("brew"), "Find Coffee")
      ),

      # Right: the pour.
      uiOutput(ns("match_ui"))
    ),

    hr(),
    p(style = "font-size:17px; color:#2B2018; max-width:860px;",
      "The ", strong("Conclusion"), " tab consolidates these findings into the ",
      "study's overall conclusions.")
  )
}

flavorServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {

    scored <- data[!is.na(data$Total.Cup.Points) & data$Total.Cup.Points > 0, ]

    # ── Feature importance: each attribute's correlation with the chosen score ──
    target_lab <- reactive(
      if ((input$target %||% "Total.Cup.Points") == "Cupper.Points")
        "the grader's overall mark" else "the total score")
    output$imp_title <- renderText(sprintf("What drives %s", target_lab()))
    output$importance <- renderPlot({
      tgt  <- input$target %||% "Total.Cup.Points"
      cors <- sapply(FLAVOR_ATTRS, function(a)
        suppressWarnings(cor(scored[[a]], scored[[tgt]], use = "complete.obs")))
      cc <- data.frame(attr = gsub("\\.", " ", FLAVOR_ATTRS), r = as.numeric(cors))
      cc <- cc[order(cc$r), ]
      cc$attr <- factor(cc$attr, levels = cc$attr)
      cc$top  <- cc$r == max(cc$r)
      ggplot(cc, aes(r, attr, fill = top)) +
        geom_col(width = 0.72) +
        geom_text(aes(label = sprintf("%.2f", r)), hjust = -0.18, size = 4,
                  colour = LATTE$subtext) +
        scale_fill_manual(values = c(`FALSE` = COFFEE_COLS$blue,
                                     `TRUE`  = COFFEE_COLS$green), guide = "none") +
        scale_x_continuous(limits = c(0, 1), expand = expansion(mult = c(0, 0.06))) +
        labs(x = "How closely it tracks the score", y = NULL) + theme_coffee()
    })

    # ── One attribute vs total score: a plain scatter you flip through, one note at ──
    # a time. With a single attribute the points don't overlap into mush, so the funnel
    # shows itself — the cloud fans wide at low notes and tightens to the top-right.
    # A light trend line just guides the eye. Clicking a point shows that coffee's
    # full breakdown on the right (see output$funnel_pick). 
    funnel_df <- reactive({
      a    <- input$funnel_attr %||% "Aftertaste"
      keep <- !is.na(scored[[a]]) & scored[[a]] > 0 &
              !is.na(scored$Total.Cup.Points) & scored$Total.Cup.Points > 0
      d <- scored[keep, , drop = FALSE]
      d$x <- d[[a]]; d$y <- d$Total.Cup.Points
      d
    })

    output$funnel <- renderPlot({
      d <- funnel_df()
      a <- input$funnel_attr %||% "Aftertaste"
      if (nrow(d) == 0)
        return(gg_no_data("No coffees to plot for that note."))
      ggplot(d, aes(x, y)) +
        geom_point(colour = COFFEE_COLS$blue, alpha = 0.35, size = 1.8) +
        geom_smooth(method = "lm", formula = y ~ x, se = FALSE,
                    colour = COFFEE_COLS$green, linewidth = 1.1) +
        labs(x = sprintf("%s score (out of 10)", gsub("\\.", " ", a)),
             y = "Total cup score (out of 100)") +
        theme_coffee()
    })

    # Remember the clicked point; forget it when the note (and so the layout) changes.
    sel_click <- reactiveVal(NULL)
    observeEvent(input$funnel_click, sel_click(input$funnel_click))
    observeEvent(input$funnel_attr,  sel_click(NULL), ignoreInit = TRUE)

    # The clicked coffee's full breakdown: its nine notes + the grader's mark,
    # as a labelled bar chart with the total in the title.
    output$funnel_pick <- renderPlot({
      cl <- sel_click()
      if (is.null(cl))
        return(gg_no_data("Click a point to see that coffee's full scores."))
      row <- nearPoints(funnel_df(), cl, xvar = "x", yvar = "y",
                        maxpoints = 1, threshold = 20)
      if (nrow(row) == 0)
        return(gg_no_data("No coffee there — click closer to a point."))
      r      <- row[1, ]
      comps  <- c(FLAVOR_ATTRS, "Cupper.Points")
      labels <- ifelse(comps == "Cupper.Points", "Grader overall", gsub("\\.", " ", comps))
      pick   <- data.frame(comp = factor(labels, levels = rev(labels)),
                           val  = vapply(comps, function(c) as.numeric(r[[c]]), numeric(1)))
      # Keep long producer names from shoving the total off the edge: cap the
      # length, wrap onto a couple of lines, and give the total its own line.
      nm <- cup_name(r)
      if (nchar(nm) > 48) nm <- paste0(substr(nm, 1, 47), "…")
      nm <- paste(strwrap(nm, width = 26), collapse = "\n")
      ggplot(pick, aes(val, comp)) +
        geom_col(fill = COFFEE_COLS$blue, width = 0.7) +
        geom_text(aes(label = sprintf("%.2f", val)), hjust = -0.15,
                  size = 4, colour = LATTE$subtext) +
        scale_x_continuous(limits = c(0, 10.8), expand = expansion(mult = c(0, 0.02))) +
        labs(title = nm, subtitle = sprintf("Total %.1f / 100", r$Total.Cup.Points),
             x = "Score (out of 10)", y = NULL) +
        theme_coffee()
    })

    # ── The coffee builder ────────────────────────────────────────────────────
    # Straightforward matching: each slider is a target score out of 10, and every
    # coffee already has a real score out of 10 on each attribute, so they compare
    # directly — no rescaling. A coffee's match is how close, on average, its
    # scores sit to the sliders.
    match_result <- eventReactive(input$brew, {
      ids   <- builder_ids()
      dials <- setNames(
        vapply(FLAVOR_ATTRS, function(a) input[[ids[[a]]]] %||% 7, numeric(1)),
        FLAVOR_ATTRS)

      d  <- scored
      ok <- Reduce(`&`, lapply(FLAVOR_ATTRS, function(a) !is.na(d[[a]]) & d[[a]] > 0))
      d  <- d[ok, ]
      if (nrow(d) == 0) return(NULL)

      # Average points a coffee sits away from the sliders, across the 9 attributes.
      gap <- vapply(FLAVOR_ATTRS, function(a) abs(d[[a]] - dials[[a]]), numeric(nrow(d)))
      if (is.null(dim(gap))) gap <- matrix(gap, nrow = 1)   # single-row edge case
      d$gap <- rowMeans(gap)
      d$fit <- 100 * (1 - d$gap / 9)          # 100% = an exact match on every attribute
      d <- d[order(d$gap, -d$Total.Cup.Points), ]

      list(picks = head(d, 3), dials = dials)
    })

    # Friendly display name for one coffee row.
    cup_name <- function(row) {
      prod <- trimws(row$Producer)
      if (!is.na(prod) && prod != "") prod
      else if (!is.na(row$Region) && row$Region != "")
        paste("A grower in", tools::toTitleCase(row$Region))
      else paste("A grower in", row$Country.of.Origin)
    }

    output$match_ui <- renderUI({
      if (input$brew == 0) {
        return(card(card_body(
          div(style = "text-align:center; padding:70px 30px; color:#6F5C49;",
              icon("mug-hot", style = "font-size:44px; color:#C68642;"),
              h4(style = "margin-top:16px;", "Set your flavour profile"),
              p(style = "max-width:400px; margin:0 auto; font-size:15px;",
                "Move the dials to the profile you want and press Make My Coffee ",
                "to see the graded coffees closest to it.")))))
      }
      res <- match_result()
      if (is.null(res)) {
        return(card(card_body(
          div(style = "text-align:center; padding:60px 30px; color:#6F5C49;",
              h4("No coffee to match"),
              p("No graded coffee has a value for every attribute.")))))
      }
      best <- res$picks[1, ]

      compare <- lapply(FLAVOR_ATTRS, function(a) {
        tags$li(style = "margin-bottom:6px;",
          sprintf("%s: you set %.1f, this coffee scores %.1f.",
                  gsub("\\.", " ", a), res$dials[[a]], best[[a]]))
      })

      runner_card <- function(row) {
        div(style = paste0("flex:1; min-width:200px; background:#FBF6EE; border:1px solid #EADDCB;",
                           "border-radius:12px; padding:14px 16px;"),
            div(style = "font-weight:650; font-size:15px; color:#2B2018;", cup_name(row)),
            div(style = "font-size:13.5px; color:#6F5C49; margin-top:2px;",
                paste0(row$Country.of.Origin, " · ", row$Processing.Method)),
            div(style = "font-size:13.5px; color:#9C5A20; font-weight:600; margin-top:6px;",
                sprintf("%.0f%% match · scored %.1f", row$fit, row$Total.Cup.Points)))
      }

      card(
        card_header(div(style = "display:flex; justify-content:space-between; align-items:center;",
                        span("Closest match"),
                        span(style = paste0("background:linear-gradient(120deg,#9C5A20,#C68642); color:#fff;",
                                            "border-radius:999px; padding:4px 14px; font-size:14px; font-weight:700;"),
                             sprintf("%.0f%% match", best$fit)))),
        card_body(
          div(style = "font-size:26px; font-weight:700; color:#2B2018;", cup_name(best)),
          div(style = "font-size:15.5px; color:#6F5C49; margin:2px 0 14px;",
              paste0(best$Country.of.Origin,
                     ifelse(!is.na(best$Region) & best$Region != "",
                            paste0(" · ", tools::toTitleCase(best$Region)), ""),
                     " · prepared ", tolower(best$Processing.Method), " style",
                     " · graded ", sprintf("%.1f", best$Total.Cup.Points), " out of 100")),

          h5(style = "font-weight:650;", "Your profile vs this coffee"),
          tags$ul(style = "font-size:14.5px; color:#2B2018; line-height:1.55; padding-left:20px;", compare),

          h5(style = "font-weight:650;", "Other close matches"),
          div(style = "display:flex; gap:14px; flex-wrap:wrap;",
              lapply(seq_len(min(2, nrow(res$picks) - 1)),
                     function(i) runner_card(res$picks[i + 1, ])))
        ))
    })
  })
}
