# Sensory Analysis tab
# --------------------
# The sensory side of the story: which of the nine tasting attributes actually
# drive the score. A toggle points every chart at either the final Total score
# or the grader's own overall mark (Cupper Points) — flipping it shows the two
# are driven by the same attributes, which is the "transparent scoring" story.
# (Growing/processing factors live on the Attributing Factors tab.)

library(bslib)

flavorUI <- function(id) {
  ns <- NS(id)
  tagList(
    h2("Sensory Analysis — which tasting notes drive the score"),
    p("A coffee's score is built from nine tasting attributes, but they don't all ",
      "matter equally. This shows which ones actually separate a great cup from an ",
      "ordinary one. Switch between the final ", strong("total score"), " and the ",
      strong("grader's own overall mark"), " — the ranking barely changes, which is ",
      "how you know the scoring is consistent rather than arbitrary."),

    radioButtons(ns("target"), "What the attributes drive",
                 choices = c("Total score"           = "Total.Cup.Points",
                             "Grader's overall mark"  = "Cupper.Points"),
                 selected = "Total.Cup.Points", inline = TRUE),

    # Two collapsible sections; the importance ranking opens first.
    accordion(
      open = "Importance — what drives the score",

      accordion_panel(
        "Importance — what drives the score",
        card(full_screen = TRUE,
             card_header(textOutput(ns("imp_title"))),
             card_body(
               plotOutput(ns("importance"), height = "360px"),
               p(class = "card-note",
                 "How closely each attribute tracks the chosen score. Aftertaste ",
                 "and flavour lead by a distance; the near-perfect attributes ",
                 "(sweetness, clean cup, uniformity) barely move it, because almost ",
                 "every coffee scores top marks on those.")))),

      accordion_panel(
        "Explore each attribute",
        card(full_screen = TRUE,
             card_header(textOutput(ns("driver_title"))),
             card_body(
               checkboxGroupInput(ns("driver_attrs"), "Sensory attributes",
                           choices = setNames(FLAVOR_ATTRS, gsub("\\.", " ", FLAVOR_ATTRS)),
                           selected = c("Aftertaste", "Flavor", "Sweetness"), inline = TRUE),
               plotOutput(ns("drivers"), height = "400px"),
               p(class = "card-note",
                 "Each line fits one attribute against the chosen score — the ",
                 "steeper the line, the more that attribute drives it. Try ",
                 "Aftertaste (steep) against Sweetness (flat). Point clouds show ",
                 "when 3 or fewer are selected."))))),

    hr(),
    p(style = "font-size:17px; color:#2B2018; max-width:860px;",
      "That's what tastes drive the score. For the headline conclusions across ",
      "the whole dataset, head to the ", strong("Summary"), " tab.")
  )
}

flavorServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {

    scored <- data[!is.na(data$Total.Cup.Points) & data$Total.Cup.Points > 0, ]

    target_lab <- reactive(
      if ((input$target %||% "Total.Cup.Points") == "Cupper.Points")
        "the grader's overall mark" else "the total score")
    y_lab <- reactive(
      if ((input$target %||% "Total.Cup.Points") == "Cupper.Points")
        "Grader's overall mark" else "Total Cup Points")

    # ── Ranked importance: each attribute's correlation with the chosen score ───
    output$imp_title <- renderText(sprintf("What drives %s", target_lab()))
    output$importance <- renderPlot({
      tgt <- input$target %||% "Total.Cup.Points"
      d   <- scored
      cors <- sapply(FLAVOR_ATTRS, function(a)
        suppressWarnings(cor(d[[a]], d[[tgt]], use = "complete.obs")))
      cc <- data.frame(attr = gsub("\\.", " ", FLAVOR_ATTRS), r = as.numeric(cors))
      cc <- cc[order(cc$r), ]
      cc$attr <- factor(cc$attr, levels = cc$attr)
      cc$top  <- cc$r == max(cc$r)
      ggplot(cc, aes(r, attr, fill = top)) +
        geom_col(width = 0.72) +
        geom_text(aes(label = sprintf("%.2f", r)), hjust = -0.18, size = 3.4,
                  colour = LATTE$subtext) +
        scale_fill_manual(values = c(`FALSE` = COFFEE_COLS$blue,
                                     `TRUE` = COFFEE_COLS$green), guide = "none") +
        scale_x_continuous(limits = c(0, 1), expand = expansion(mult = c(0, 0.06))) +
        labs(x = "How closely it tracks the score", y = NULL) + theme_coffee()
    })

    # ── Detail scatter: chosen attributes vs the chosen score ───────────────────
    output$driver_title <- renderText(sprintf("Each attribute vs %s", target_lab()))
    output$drivers <- renderPlot({
      tgt   <- input$target %||% "Total.Cup.Points"
      attrs <- input$driver_attrs
      if (is.null(attrs) || length(attrs) == 0)
        return(gg_no_data("Tick at least one attribute to compare."))
      d <- scored
      long <- do.call(rbind, lapply(attrs, function(a) {
        sub <- d[!is.na(d[[a]]) & !is.na(d[[tgt]]), ]
        if (nrow(sub) < 3) return(NULL)
        data.frame(score = sub[[a]], total = sub[[tgt]],
                   attr = gsub("\\.", " ", a), stringsAsFactors = FALSE)
      }))
      if (is.null(long) || nrow(long) == 0) return(gg_no_data())
      # Order the legend strongest-first (by correlation with the target).
      rord <- sapply(attrs, function(a)
        suppressWarnings(cor(d[[a]], d[[tgt]], use = "complete.obs")))
      long$attr <- factor(long$attr, levels = gsub("\\.", " ", attrs[order(-rord)]))
      pal <- cat_cols(nlevels(long$attr))
      p <- ggplot(long, aes(score, total, colour = attr))
      if (length(attrs) <= 3) p <- p + geom_point(alpha = 0.3, size = 1.6)
      p + geom_smooth(method = "lm", se = FALSE, linewidth = 1.1) +
        scale_colour_manual(values = pal) +
        labs(x = "Attribute score (0–10)", y = y_lab(), colour = NULL) +
        theme_coffee()
    })
  })
}
