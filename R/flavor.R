# Sensory Analysis tab — what drives the score, then build your own coffee
# -------------------------------------------------------------------------
# Two halves:
#   1. Feature importance — every sensory attribute ranked by how closely it
#      tracks the score (total score or the grader's overall mark).
#   2. Make your own coffee — word-labelled sliders for what you love,
#      a preparation style, and one big button. We match your palate against
#      every graded coffee and pour out the closest real cup, with reasons.

library(bslib)

# The dials the coffee builder matches on (Sweetness is near-identical across
# the dataset, so Flavor richness separates coffees far better).
BUILD_ATTRS <- c(Brightness = "Acidity", Body = "Body",
                 Richness = "Flavor", Smoothness = "Balance")

# A slider label with a plain-word scale beneath it, so the 1-10 dials read as
# taste choices (low -> high) without any JavaScript.
dial_label <- function(title, low, high) {
  tagList(strong(title),
          span(style = "color:#8A7965; font-weight:400; font-size:12px;",
               sprintf(" (%s → %s)", low, high)))
}

flavorUI <- function(id) {
  ns <- NS(id)
  tagList(
    h2("Sensory Analysis"),
    p(style = "max-width:860px; font-size:17px; color:#444; line-height:1.6;",
      "Which tasting notes actually decide a coffee's score? This ranks every ",
      "sensory attribute by how closely it tracks the score — the longer the bar, ",
      "the more it drives quality. Switch between the final total score and the ",
      "grader's own overall mark; the ranking barely changes, which is how you know ",
      "the scoring is consistent. Then scroll down to build your own cup."),

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

    # ── 2. Make your own coffee ─────────────────────────────────────────────
    h4("Make your own coffee"),
    p(style = "max-width:860px; font-size:15px; color:#444; line-height:1.6;",
      "Play barista. Set the four dials to your taste, choose how you like your ",
      "beans prepared, and press the button. We compare your palate against ",
      "every professionally graded coffee here and pour out your closest match."),

    layout_columns(
      col_widths = c(4, 8),

      # Left sidebar: the preference dials (1-10, low -> high word scale).
      wellPanel(class = "taste-dials",
        h5(style = "margin-top:0; font-weight:600;", "Your taste"),
        sliderInput(ns("p_bright"), dial_label("Brightness", "soft and mellow", "zingy"),
                    min = 1, max = 10, value = 7, step = 0.5, ticks = FALSE),
        sliderInput(ns("p_body"), dial_label("Body", "feather light", "full and bold"),
                    min = 1, max = 10, value = 6, step = 0.5, ticks = FALSE),
        sliderInput(ns("p_rich"), dial_label("Flavour richness", "delicate", "intense"),
                    min = 1, max = 10, value = 7, step = 0.5, ticks = FALSE),
        sliderInput(ns("p_smooth"), dial_label("Smoothness", "a little edgy", "silky"),
                    min = 1, max = 10, value = 6, step = 0.5, ticks = FALSE),
        radioButtons(ns("p_style"), "How your beans are prepared",
                     choices = c("Bright and clean (washed)"   = "Washed / Wet",
                                 "Fruity and bold (natural)"   = "Natural / Dry",
                                 "Smooth and mellow (semi washed)" = "Semi-washed / Semi-pulped",
                                 "Surprise me"                  = "any"),
                     selected = "any"),
        actionButton(ns("brew"), "Make My Coffee", class = "btn-brew",
                     icon = icon("mug-hot"))
      ),

      # Right: the pour.
      uiOutput(ns("match_ui"))
    ),

    hr(),
    p(style = "font-size:17px; color:#2B2018; max-width:860px;",
      "Found your match? The ", strong("Summary"), " tab wraps the whole story ",
      "up in one page.")
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

    # ── The coffee builder ────────────────────────────────────────────────────
    # Map a 1-10 dial onto each attribute's real 5th-95th percentile range, so
    # dial 1 means "as mellow as coffees actually get" rather than an impossible
    # zero, and dial 10 means "as intense as the very top of the field".
    attr_range <- lapply(BUILD_ATTRS, function(a) {
      v <- scored[[a]]; v <- v[!is.na(v) & v > 0]
      quantile(v, c(0.05, 0.95), names = FALSE)
    })
    dial_to_target <- function(dial, rng) rng[1] + (dial - 1) / 9 * (rng[2] - rng[1])

    match_result <- eventReactive(input$brew, {
      dials <- c(Brightness = input$p_bright, Body = input$p_body,
                 Richness = input$p_rich, Smoothness = input$p_smooth)
      targets <- mapply(function(nm, a) dial_to_target(dials[[nm]], attr_range[[nm]]),
                        names(BUILD_ATTRS), BUILD_ATTRS)
      names(targets) <- names(BUILD_ATTRS)

      d <- scored
      if (input$p_style != "any") d <- d[d$Processing.Method == input$p_style, ]
      cols <- unname(BUILD_ATTRS)
      ok <- Reduce(`&`, lapply(cols, function(a) !is.na(d[[a]]) & d[[a]] > 0))
      d <- d[ok, ]
      if (nrow(d) == 0) return(NULL)

      # Distance in units of each attribute's spread, so no dial dominates.
      sds <- vapply(cols, function(a) sd(scored[[a]], na.rm = TRUE), numeric(1))
      gap <- mapply(function(nm, a, s) ((d[[a]] - targets[[nm]]) / s)^2,
                    names(BUILD_ATTRS), BUILD_ATTRS, sds)
      if (is.null(dim(gap))) gap <- matrix(gap, nrow = 1)   # single-row edge case
      dist <- rowSums(gap)
      d$fit <- 100 * exp(-dist / 2)               # 100 = spot on, fades with distance
      d <- d[order(-d$fit, -d$Total.Cup.Points), ]

      # Popularity: how this coffee's shipped bags compare with the whole field.
      pop_pct <- function(bags) round(100 * mean(scored$Number.of.Bags <= bags, na.rm = TRUE))
      picks <- head(d, 3)
      picks$pop <- vapply(picks$Number.of.Bags, pop_pct, numeric(1))
      list(picks = picks, targets = targets, dials = dials)
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
              h4(style = "margin-top:16px;", "Your cup is waiting"),
              p(style = "max-width:400px; margin:0 auto; font-size:15px;",
                "Set the dials to your taste and press Make My Coffee. ",
                "We will find the real graded coffee that fits your palate best.")))))
      }
      res <- match_result()
      if (is.null(res)) {
        return(card(card_body(
          div(style = "text-align:center; padding:60px 30px; color:#6F5C49;",
              h4("No beans match that combination"),
              p("Try a different preparation style, or choose Surprise me.")))))
      }
      best <- res$picks[1, ]
      dial_words <- c(Brightness = "brightness", Body = "body",
                      Richness = "flavour richness", Smoothness = "smoothness")

      why <- lapply(names(BUILD_ATTRS), function(nm) {
        a <- BUILD_ATTRS[[nm]]
        tags$li(style = "margin-bottom:6px;",
          sprintf("You set %s to %.1f, which means a score near %.1f. This cup pours %.1f.",
                  dial_words[[nm]], res$dials[[nm]], res$targets[[nm]], best[[a]]))
      })

      pop_line <- if (best$pop >= 70)
        sprintf("A proven crowd pleaser: more bags of this coffee shipped than %d%% of every coffee here.", best$pop)
      else if (best$pop >= 40)
        sprintf("Comfortably popular: it out-ships %d%% of the coffees in this dataset.", best$pop)
      else
        sprintf("A hidden gem: only a small harvest (more bags shipped by %d%% of the field), so consider it a connoisseur's pick.", 100 - best$pop)

      runner_card <- function(row) {
        div(style = paste0("flex:1; min-width:200px; background:#FBF6EE; border:1px solid #EADDCB;",
                           "border-radius:12px; padding:14px 16px;"),
            div(style = "font-weight:650; font-size:15px; color:#2B2018;", cup_name(row)),
            div(style = "font-size:13.5px; color:#6F5C49; margin-top:2px;",
                paste0(row$Country.of.Origin, " · ", row$Processing.Method)),
            div(style = "font-size:13.5px; color:#9C5A20; font-weight:600; margin-top:6px;",
                sprintf("%.0f%% fit · scored %.1f", row$fit, row$Total.Cup.Points)))
      }

      card(
        card_header(div(style = "display:flex; justify-content:space-between; align-items:center;",
                        span("Your custom blend match"),
                        span(style = paste0("background:linear-gradient(120deg,#9C5A20,#C68642); color:#fff;",
                                            "border-radius:999px; padding:4px 14px; font-size:14px; font-weight:700;"),
                             sprintf("%.0f%% fit", best$fit)))),
        card_body(
          div(style = "font-size:26px; font-weight:700; color:#2B2018;", cup_name(best)),
          div(style = "font-size:15.5px; color:#6F5C49; margin:2px 0 14px;",
              paste0(best$Country.of.Origin,
                     ifelse(!is.na(best$Region) & best$Region != "",
                            paste0(" · ", tools::toTitleCase(best$Region)), ""),
                     " · prepared ", tolower(best$Processing.Method), " style",
                     " · graded ", sprintf("%.1f", best$Total.Cup.Points), " out of 100")),

          h5(style = "font-weight:650;", "Why this fits your palate"),
          tags$ul(style = "font-size:14.5px; color:#2B2018; line-height:1.55; padding-left:20px;", why),

          div(style = paste0("background:#FBF6EE; border-left:4px solid #C68642; border-radius:8px;",
                             "padding:10px 14px; font-size:14.5px; color:#2B2018; margin:8px 0 16px;"),
              pop_line),

          h5(style = "font-weight:650;", "Two more cups worth a try"),
          div(style = "display:flex; gap:14px; flex-wrap:wrap;",
              lapply(seq_len(min(2, nrow(res$picks) - 1)),
                     function(i) runner_card(res$picks[i + 1, ])))
        ))
    })
  })
}
