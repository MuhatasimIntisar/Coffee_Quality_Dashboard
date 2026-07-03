library(shiny)
library(bslib)

# Each tab is a Shiny module defined in its own file under R/ (auto-sourced).
# This file lays out the overall page, applies the coffee theme, and slots each
# module's UI in. Flow: orient (Introduction) -> explore the world (Global) ->
# dig into a slice (Profile) -> analyse drivers (Analysis) -> conclude.

# ── Brand palette ─────────────────────────────────────────────────────────────
# Shared with R/helpers.R (COFFEE_COLS). Defined here too so the theme can use it.
coffee_colours <- c(
  espresso = "#3A2417",  # near-black warm ink
  roast    = "#8A5A2B",  # roasted brown  (primary data colour)
  caramel  = "#C68642",  # caramel        (highlight)
  latte    = "#E8C99A",  # latte
  cream    = "#F7F1E7",  # page background
  green    = "#2E8B74"   # fresh green    (positive highlights)
)

# Coffee palette mapped onto Bootstrap's semantic colours. Typography uses the
# native system-font stack (Bootstrap 5 default) — no web fonts to download.
coffee_theme <- bs_theme(
  version      = 5,
  bg           = "#F7F1E7",                # light cream base
  fg           = "#2B2018",                # higher-contrast ink (was #4E2A04)
  primary      = coffee_colours[["roast"]],
  secondary    = coffee_colours[["espresso"]],
  success      = coffee_colours[["green"]],
  info         = coffee_colours[["caramel"]],
  warning      = coffee_colours[["caramel"]],
  danger       = "#9E2B25",
  # Fonts: native system-font stack (no download), sized up a notch overall.
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
      --ink:#2B2018; --muted:#6F5C49; --accent:#8A5A2B; --caramel:#C68642;
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

    /* ── Tabs: underline style (replaces filled active tab) ─── */
    .nav-tabs{ border-bottom:1px solid var(--line); gap:4px; margin-bottom:22px; }
    .nav-tabs .nav-link{ color:var(--muted); border:none; border-bottom:2px solid transparent;
                         font-weight:500; padding:12px 16px; }
    .nav-tabs .nav-link:hover{ color:var(--ink); border-bottom-color:var(--line); }
    .nav-tabs .nav-link.active{ color:var(--ink); font-weight:600; background:transparent;
                                border-bottom:2px solid var(--accent); }

    /* ── wellPanel -> clean control card (replaces tan .well) ─ */
    .well{ background:var(--surface); border:1px solid var(--line); border-radius:14px;
           box-shadow:0 1px 2px rgba(58,36,23,.05); padding:18px 20px; }

    /* ── Inputs ─────────────────────────────────────────────── */
    .form-control, .form-select, .selectize-input{ border-color:var(--line)!important;
           border-radius:10px!important; }
    .selectize-input.focus, .form-control:focus, .form-select:focus{
           border-color:var(--accent)!important; box-shadow:0 0 0 3px rgba(138,90,43,.12)!important; }
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
      h1("Coffee Quality Dashboard", class = "app-title"),
      actionLink("about_btn", "About", icon = icon("circle-info"))),

  tabsetPanel(
    id = "tabs",
    type = "tabs",

    tabPanel("Overview",            introductionUI("introduction")),
    tabPanel("Global",              locationUI("location")),
    tabPanel("Profile",             profileUI("profile")),
    tabPanel("Attributing Factors", analysisUI("analysis")),
    tabPanel("Sensory Analysis",    flavorUI("flavor")),
    tabPanel("Summary",             conclusionUI("conclusion"))
  )
)
