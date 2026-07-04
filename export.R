# export.R — build the static (WebAssembly) HTML version for submission.
# ---------------------------------------------------------------------------
# A Shiny app normally needs a live R server, so it is not a static HTML file.
# shinylive compiles the app to WebAssembly and writes a self-contained "site/"
# folder that runs the WHOLE app in the browser with no server — this folder is
# the "HTML output" the brief asks for.
#
# Run this from the project root (the folder holding global.R / ui.R / server.R).

# 1. Install the exporter (first time only).
if (!requireNamespace("shinylive", quietly = TRUE)) install.packages("shinylive")

# 2. shinylive bundles EVERYTHING in the app directory, so build from a clean
#    copy that contains only the app itself — not renv/, the PDFs, or the
#    "R dashboard styling tips" folder (those would bloat or break the export).
app <- "app_build"
unlink(app, recursive = TRUE)
dir.create(app)
file.copy(c("global.R", "ui.R", "server.R", "Group5_coffee.csv"), app)
dir.create(file.path(app, "R"))
file.copy(list.files("R", full.names = TRUE), file.path(app, "R"))

# 3. Export the clean app to ./site
shinylive::export(appdir = app, destdir = "site")

# 4. Preview it. A shinylive site must be SERVED over http — opening index.html
#    directly with file:// will not work. Use either:
cat("\nExport complete.\nPreview the HTML output with ONE of:\n",
    "  R:     httpuv::runStaticServer('site')\n",
    "  shell: cd site && python3 -m http.server 8080   (then open http://localhost:8080)\n\n",
    "Then click through every tab and confirm the globe, tables and\n",
    "click interactions still work before submitting.\n", sep = "")
