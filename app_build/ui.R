library(shiny)
library(bslib)

# Each tab is a Shiny module defined in its own file under R/ (auto-sourced).
# This file lays out the overall page, applies the coffee design system, and
# slots each module's UI in. Flow: welcome hub (Overview) -> spin the world
# (Global) -> taste one origin (Profile) -> what shapes the score
# (Attributing Factors) -> feel it and build your own (Sensory Analysis) ->
# the big picture (Conclusion).

# ── Brand palette ─────────────────────────────────────────────────────────────
# Shared with R/helpers.R (COFFEE_COLS). Defined here too so the theme can use it.
coffee_colours <- c(
  espresso = "#3A2417",  # near-black warm ink
  roast    = "#9C5A20",  # roasted brown  (primary data colour)
  caramel  = "#C68642",  # caramel        (highlight)
  latte    = "#E8C99A",  # latte
  cream    = "#F7F1E7",  # page background
  green    = "#0E8A63"   # fresh green    (positive highlights)
)

# Coffee palette mapped onto Bootstrap's semantic colours. Typography uses the
# native system-font stack (Bootstrap 5 default) — no web fonts to download.
coffee_theme <- bs_theme(
  version      = 5,
  bg           = "#F7F1E7",                # light cream base
  fg           = "#2B2018",                # high-contrast warm ink
  primary      = coffee_colours[["roast"]],
  secondary    = coffee_colours[["espresso"]],
  success      = coffee_colours[["green"]],
  info         = coffee_colours[["caramel"]],
  warning      = coffee_colours[["caramel"]],
  danger       = "#9E2B25",
  "font-size-base"    = "1.05rem",
  "border-radius"     = "12px",
  "card-border-color" = "#EADDCB"
)

ui <- fluidPage(
  theme = coffee_theme,
  lang  = "en",

  tags$head(tags$style(HTML("
    :root{
      --cream:#F7F1E7; --surface:#FFFFFF; --line:#EADDCB;
      --ink:#2B2018; --muted:#6F5C49; --accent:#9C5A20; --caramel:#C68642;
    }
    body{ background:var(--cream); color:var(--ink); }
    .container-fluid{ max-width:100%; padding-left:max(2rem,3%); padding-right:max(2rem,3%); }

    /* ── App header ──────────────────────────────────────────── */
    .app-header{ padding:22px 0 18px; border-bottom:1px solid var(--line); margin-bottom:22px; }
    .app-kicker{ font-family:ui-monospace,Consolas,'Liberation Mono',monospace; font-size:12px; letter-spacing:.2em;
                 text-transform:uppercase; color:var(--accent); margin-bottom:6px; }
    .app-title{ font-family:system-ui,-apple-system,'Segoe UI',Roboto,Helvetica,Arial,sans-serif; font-weight:600; letter-spacing:-.02em;
                font-size:34px; margin:0; color:var(--ink); }

    /* ── Headings & lead text ───────────────────────────────── */
    h2{ font-family:system-ui,-apple-system,'Segoe UI',Roboto,Helvetica,Arial,sans-serif; font-weight:600; letter-spacing:-.01em;
        font-size:28px; color:var(--ink); }
    h4{ font-family:system-ui,-apple-system,'Segoe UI',Roboto,Helvetica,Arial,sans-serif; font-weight:600; font-size:20px;
        margin-top:6px; color:var(--ink); }
    .tab-pane > p:first-of-type{ color:var(--muted); max-width:72ch; font-size:17px; line-height:1.6; }

    a, a:hover{ text-decoration:none; }


    /* ── Overview hub cards (clickable gateways into the tabs) ── */
    .hub-grid{ display:grid; grid-template-columns:repeat(auto-fit, minmax(240px, 1fr));
               gap:18px; margin:8px 0 6px; }
    .hub-card{ position:relative; display:block; background:var(--surface); border:1px solid var(--line);
               border-radius:16px; padding:22px 22px 18px; cursor:pointer; overflow:hidden;
               box-shadow:0 1px 2px rgba(58,36,23,.05); text-decoration:none; }
    .hub-card:hover, .hub-card:focus{ text-decoration:none; }
    .hub-card:hover{ box-shadow:0 14px 30px rgba(58,36,23,.14); border-color:var(--caramel); }
    .hub-card::after{ content:''; position:absolute; inset:0 0 auto 0; height:4px;
                      background:var(--hub-accent, var(--accent)); }
    .hub-kicker{ font-family:ui-monospace,Consolas,'Liberation Mono',monospace; font-size:11px; letter-spacing:.14em;
                 text-transform:uppercase; color:var(--hub-accent, var(--accent)); margin:2px 0 10px; }
    .hub-big{ font-size:30px; font-weight:650; line-height:1.05; color:var(--ink); margin:0 0 8px; }
    .hub-desc{ font-size:14.5px; color:var(--muted); line-height:1.5; margin:0 0 14px; min-height:44px; }
    .hub-go{ font-size:14px; font-weight:600; color:var(--hub-accent, var(--accent));
             display:inline-flex; align-items:center; gap:6px; }

    /* ── Explainer blocks under charts (Attributing Factors) ── */
    .explain{ background:#FBF6EE; border:1px solid var(--line); border-radius:12px;
              padding:14px 18px; margin-top:12px; }
    .explain .xrow{ display:flex; gap:10px; margin:6px 0; align-items:baseline; }
    .explain .xtag{ flex:none; width:130px; font-family:ui-monospace,Consolas,monospace; font-size:11px;
                    letter-spacing:.08em; text-transform:uppercase; color:var(--accent); font-weight:600; }
    .explain .xtxt{ font-size:14.5px; color:var(--ink); line-height:1.55; }

    /* ── Conclusion finding cards ────────────────────────────── */
    .takeaway-grid{ display:grid; grid-template-columns:repeat(auto-fit, minmax(270px, 1fr));
                    gap:18px; margin:8px 0 6px; }
    .takeaway-card{ position:relative; background:var(--surface); border:1px solid var(--line);
                    border-radius:16px; padding:20px 22px 18px; overflow:hidden;
                    box-shadow:0 1px 2px rgba(58,36,23,.05); }
    .takeaway-card:hover{ box-shadow:0 14px 30px rgba(58,36,23,.14); border-color:var(--caramel); }
    .takeaway-card::before{ content:''; position:absolute; inset:0 auto 0 0; width:5px;
                    background:var(--tk-accent, var(--accent)); }
    .takeaway-num{ font-family:ui-monospace,Consolas,monospace; font-size:12px; font-weight:700;
                    letter-spacing:.12em; color:var(--tk-accent, var(--accent)); margin-bottom:8px; }
    .takeaway-title{ font-size:19px; font-weight:700; color:var(--ink); margin:0 0 8px; line-height:1.25; }
    .takeaway-body{ font-size:14.5px; color:#4A3B2C; line-height:1.6; margin:0; }

    /* ── Pop-out slice callout (Profile) ────────────────────── */
    .slice-callout{ background:var(--surface); border:1px solid var(--caramel); border-radius:14px;
                    padding:16px 20px; box-shadow:0 12px 30px rgba(58,36,23,.16); }
    .slice-pct{ font-size:34px; font-weight:700; color:var(--accent); line-height:1; }
    .mini-rating{ display:inline-block; background:#FBF6EE; border:1px solid var(--line); border-radius:10px;
                  padding:8px 14px; margin:6px 8px 0 0; text-align:center; }
    .mini-rating .mr-name{ font-family:ui-monospace,Consolas,monospace; font-size:10.5px; letter-spacing:.1em;
                  text-transform:uppercase; color:var(--muted); }
    .mini-rating .mr-val{ font-size:19px; font-weight:700; color:var(--ink); }
    .mini-rating .mr-word{ font-size:12px; color:var(--accent); font-weight:600; }

    /* ── Tabs: underline style ──────────────────────────────── */
    .nav-tabs{ border-bottom:1px solid var(--line); gap:4px; margin-bottom:22px; }
    .nav-tabs .nav-link{ color:var(--muted); border:none; border-bottom:2px solid transparent;
                         font-weight:500; padding:12px 16px; }
    .nav-tabs .nav-link:hover{ color:var(--ink); border-bottom-color:var(--line); }
    .nav-tabs .nav-link.active{ color:var(--ink); font-weight:600; background:transparent;
                                border-bottom:2px solid var(--accent); }

    /* ── wellPanel -> clean control card ────────────────────── */
    .well{ background:var(--surface); border:1px solid var(--line); border-radius:14px;
           box-shadow:0 1px 2px rgba(58,36,23,.05); padding:18px 20px; }

    /* ── Inputs ─────────────────────────────────────────────── */
    .form-control, .form-select, .selectize-input{ border-color:var(--line)!important;
           border-radius:10px!important; }
    .selectize-input.focus, .form-control:focus, .form-select:focus{
           border-color:var(--accent)!important; box-shadow:0 0 0 3px rgba(156,90,32,.12)!important; }
    label, .control-label{ font-family:ui-monospace,Consolas,'Liberation Mono',monospace; font-size:13px;
           letter-spacing:.08em; text-transform:uppercase; color:var(--muted); }

    /* ── Sliders (ionRangeSlider) in coffee accent ──────────── */
    .irs--shiny .irs-bar{ background:var(--accent); border-color:var(--accent); }
    .irs--shiny .irs-handle{ border:2px solid var(--accent); }
    .irs--shiny .irs-single, .irs--shiny .irs-from, .irs--shiny .irs-to{ background:var(--accent); }
    .irs--shiny .irs-line{ background:#EFE4D2; }

    /* ── DataTables ─────────────────────────────────────────── */
    table.dataTable thead th{ border-bottom:2px solid var(--line)!important;
      font-family:ui-monospace,Consolas,'Liberation Mono',monospace; font-size:12px; letter-spacing:.06em;
      text-transform:uppercase; color:var(--muted)!important; }
    table.dataTable tbody td{ color:var(--ink); }
    table.dataTable.stripe tbody tr.odd, table.dataTable.display tbody tr.odd{ background:#FBF6EE; }
    .dataTables_wrapper .dataTables_filter input,
    .dataTables_wrapper .dataTables_length select{ border:1px solid var(--line); border-radius:8px; }

    /* ── bslib cards & sidebar ──────────────────────────────── */
    .card{ border:1px solid var(--line); border-radius:14px;
           box-shadow:0 1px 2px rgba(58,36,23,.05); }
    .card-header{ background:transparent; border-bottom:1px solid var(--line);
           font-family:system-ui,-apple-system,'Segoe UI',Roboto,Helvetica,Arial,sans-serif; font-weight:600; font-size:17px;
           color:var(--ink); padding:14px 18px; }
    .card-note{ color:var(--muted); font-size:15px; margin:0 0 10px; }
    .bslib-sidebar-layout > .sidebar{ background:var(--surface);
           border-right:1px solid var(--line); }
    .bslib-sidebar-layout .sidebar-title{ font-family:system-ui,-apple-system,'Segoe UI',Roboto,Helvetica,Arial,sans-serif;
           font-weight:600; color:var(--ink); letter-spacing:0; text-transform:none; font-size:16px; }

    /* ── Misc ───────────────────────────────────────────────── */
    hr{ border-top:1px solid var(--line); opacity:1; margin:26px 0; }
  "))),

  div(style = "display:flex; align-items:center; justify-content:space-between; margin:8px 2px 2px;",
      h1("Global Coffee Quality Assessment Platform", class = "app-title"),
      actionLink("about_btn", "About", icon = icon("circle-info"))),

  tabsetPanel(
    id = "tabs",
    type = "tabs",

    tabPanel("Overview",            value = "Overview",            introductionUI("introduction")),
    tabPanel("Global",              value = "Global",              locationUI("location")),
    tabPanel("Profile",             value = "Profile",             toneUI("tone")),
    tabPanel("Attributing Factors", value = "Attributing Factors", analysisUI("analysis")),
    tabPanel("Sensory Analysis",    value = "Sensory Analysis",    flavorUI("flavor")),
    tabPanel("Conclusion",          value = "Conclusion",          conclusionUI("conclusion"))
  )
)
