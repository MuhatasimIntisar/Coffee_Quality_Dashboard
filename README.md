# Coffee Quality Dashboard

**DSA8045 – Applied Analytics | Assignment 1**  
**Group 5**

## Overview

An interactive R Shiny dashboard exploring coffee quality data from professional coffee tasters. The dashboard allows users to explore quality scores, origin information, processing methods, and physical characteristics across 771 coffee samples.

## Dataset

`Group5_coffee.csv` — 771 observations, 24 variables.

Each row is a coffee sample evaluated by professional tasters. Key variable groups:

- **Origin**: Species, Country.of.Origin, Region, Producer, Harvest.Year
- **Processing**: Processing.Method, Number.of.Bags, In.Country.Partner, Grading.Date
- **Quality Scores**: Aroma, Flavor, Aftertaste, Acidity, Body, Balance, Uniformity, Clean.Cup, Sweetness, Cupper.Points, Total.Cup.Points
- **Physical**: Moisture, altitude_low_meters, altitude_high_meters, altitude_mean_meters

## Project Structure

```
Coffee_Quality_Dashboard/
├── ui.R                        # Dashboard layout and UI components
├── server.R                    # Reactive logic and visualisations
├── Group5_coffee.csv           # Dataset
├── Coffee_Quality_Dashboard.Rproj
├── renv/                       # Package environment (renv)
├── renv.lock                   # Locked package versions
├── Assignment 1.pdf            # Assignment brief
└── Assignment 1 datasets.pdf   # Dataset descriptions
```

## Assignment Requirements

The dashboard must include:

- Minimum 3 tabs/pages
- At least 3 interactive visualisations (using ggplot2)
- At least 2 user input controls (dropdowns, sliders, checkboxes)
- At least 2 reactive features
- Introduction section and conclusions/recommendations section
- Clear labels, legends, and axis titles throughout

**Submission deadline:** Sunday 5th July, 23:59  
**Presentation:** Monday 6th July, 10:00

**What to submit:**
- R Shiny code
- HTML output
- Peer assessment form (submitted individually)

**Assessment breakdown:** Dashboard & Design (40%) | Presentation (40%) | Peer Assessment (20%)

## Running the App

Open the project in RStudio by double-clicking `Coffee_Quality_Dashboard.Rproj`, then either:

- Click the **Run App** button at the top of `ui.R` or `server.R`
- Or run in the R console:

```r
shiny::runApp()
```

## Dependencies

This project uses `renv` for package management. To restore the package environment:

```r
renv::restore()
```

Key packages: `shiny`, `ggplot2`, `dplyr`
