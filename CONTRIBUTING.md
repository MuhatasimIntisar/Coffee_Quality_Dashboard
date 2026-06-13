# How to work on the dashboard

We split the app into **5 independent pages** so everyone can work in their own
file without merge conflicts. This guide explains how the pieces fit together
and what you need to do to build your tab.

## Who owns what

| Tab          | File                  |
| ------------ | --------------------- |
| Introduction | `R/introduction.R`    |
| Geography    | `R/geography.R`       |
| Flavor       | `R/flavor.R`          |
| Production   | `R/production.R`      |
| Conclusion   | `R/conclusion.R`      |

Put your name in the `# Owner:` line at the top of your file. **Only edit your
own file.** The shared files (`global.R`, `ui.R`, `server.R`) rarely need to
change — coordinate with the group before touching them.

## How the app is wired

```
global.R   →  loads Group5_coffee.csv ONCE into a variable called `coffee`
ui.R       →  draws the left-hand tab bar, slots in each page's UI
server.R   →  starts each page's logic, handing it the `coffee` data
R/*.R      →  one file per tab (this is where you work)
```

When the app starts: `global.R` runs first, then `ui.R` builds the layout,
then `server.R` connects everything. Files in `R/` are loaded automatically —
you never need to `source()` them.

## Anatomy of a tab file

Each file has **two functions**: one for the UI (what you see) and one for the
server (the logic behind it). Here is `R/geography.R`:

```r
geographyUI <- function(id) {
  ns <- NS(id)
  tagList(
    h2("Geography")
    # your UI goes here
  )
}

geographyServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    # your logic goes here; `data` is the coffee dataset
  })
}
```

## The ONE rule you must follow: namespacing

Because all 5 tabs share the same app, two people could accidentally name an
output `"myPlot"` and break each other. We prevent that with **namespacing**.

- In the **UI** function, wrap every input/output id in `ns(...)`:

  ```r
  plotOutput(ns("myPlot"))
  selectInput(ns("country"), "Country:", choices = unique(data$Country.of.Origin))
  ```

- In the **server** function, you do **not** use `ns()` — refer to ids directly:

  ```r
  output$myPlot <- renderPlot({ ... })      # matches ns("myPlot") in the UI
  input$country                              # matches ns("country") in the UI
  ```

That's it: `ns()` in the UI, plain ids in the server.

## Using the data

The dataset is already loaded and passed to you as `data`. Do **not** call
`read.csv` yourself — that would load the file again. Just use `data`:

```r
geographyServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    output$countryCount <- renderPlot({
      barplot(table(data$Country.of.Origin))
    })
  })
}
```

Useful columns: `Country.of.Origin`, `Region`, `Total.Cup.Points`, `Aroma`,
`Flavor`, `Aftertaste`, `Acidity`, `Body`, `Balance`, `Processing.Method`,
`Harvest.Year`, `altitude_mean_meters`.

## A complete worked example

A dropdown that filters the data and updates a plot — paste this into your file
to see the pattern end to end:

```r
geographyUI <- function(id) {
  ns <- NS(id)
  tagList(
    h2("Geography"),
    selectInput(ns("country"), "Country:", choices = NULL),
    plotOutput(ns("scorePlot"))
  )
}

geographyServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    # populate the dropdown from the data
    updateSelectInput(session, "country",
                      choices = sort(unique(data$Country.of.Origin)))

    output$scorePlot <- renderPlot({
      req(input$country)
      subset <- data[data$Country.of.Origin == input$country, ]
      hist(subset$Total.Cup.Points,
           main = input$country, xlab = "Total Cup Points")
    })
  })
}
```

## Running the app

From RStudio, open the project and click **Run App**, or in the R console:

```r
shiny::runApp()
```

## If you need a new package

Add it with `renv::install("packagename")`, then run `renv::snapshot()` so the
`renv.lock` file records it for everyone. Commit the updated `renv.lock`.

## Workflow checklist

1. `git pull` before you start.
2. Edit only your file in `R/`.
3. Run the app and check your tab works.
4. Commit and push.
