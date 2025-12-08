
<!-- README.md is generated from README.Rmd. Please edit this file -->

# `ram` Package

This repository provides:

1.  An R package **`ram`** for Resource Allocation Modeling using linear
    programming, featuring sensitivity analysis via
    `lpSolve::compute.sens = TRUE`.
2.  A `{golem}`-based Shiny app with dynamic UI modes (“solve” &
    “builder”), interactive tables, visualizations, and export
    capabilities.

------------------------------------------------------------------------

# Installation

Install the development version from GitHub:

``` r
# install.packages("remotes")
remotes::install_github("PAS-AGRO-PAS/ram")
```

Then load the package:

``` r
library(ram)
```

------------------------------------------------------------------------

# Quick Start: Launch the App

Use the unified launcher with mode control:

``` r
ram::run_app()                 # default "solve" mode
ram::run_app(mode = "builder") # explicitly launch "builder" mode
```

The app uses `with_golem_options()` to pass the mode correctly and
initializes external resources (logo, theming) automatically.

## Builder → Solver workflow

- In **builder** mode, define your resources and activities, then click
  **Open In Solver** in the sidebar.
- A new **solve** tab opens pre-loaded with your builder tables; just
  hit **Solve Model** to compute results.
- The handoff keeps column alignment intact, so custom resource names
  and coefficients carry over without extra CSV steps.

------------------------------------------------------------------------

# App Highlights

- **Dynamic theming** via `{bslib}` with brand-based colors and
  light/dark toggle.
- **Dynamic logo** rendered via `uiOutput()` so it can react to future
  theme changes.
- **Solve Mode**: upload or edit CSVs (inline edits now update the
  solver inputs), define objectives, run the LP, view results &
  sensitivity, and explore interactive plots.
- **Data validation & feedback**: CSV uploads are checked for required
  columns, numeric values, and constraint directions, while solver
  failures show actionable notifications plus downloadable lpSolve
  reports.
- **Builder Mode**: define resources and activities on-the-fly;
  resource/activity tables stay column-synced automatically so CSV/XLSX
  exports remain consistent, and a one-click “Open in Solver” handoff
  reuses the same Shiny session.
- **Custom resource support**: solver outputs now adapt to whatever
  resource columns you define, so the optimization summary table always
  matches your model.

------------------------------------------------------------------------

# For reviewers (JOSS/readers)

- Install: `remotes::install_github("PAS-AGRO-PAS/ram")`
- Run tests: `testthat::test_local()` (from the repo root)
- Launch app: `ram::run_app(mode = "solve")` and
  `ram::run_app(mode = "builder")`
- Example data: defaults load automatically; additional scenarios live
  in `vignettes/`.

------------------------------------------------------------------------

# Programmatic Usage Example

## Define the resources

``` r
library(ram)
res <- define_resources(
  resources    = c("land","labor","nitrogen",
                   "oat_min","oat_max",
                   "barley_min","barley_max",
                   "lupin_min","lupin_max",
                   "fava_min","fava_max",
                   "pasture_min","pasture_max"),
  availability = c(15, 350, 500,   2,8,       1,6,       2,5,       1,3,       2,4),
  direction    = c("<=","<=","<=", ">=","<=", ">=","<=", ">=","<=", ">=","<=", ">=","<=")
)
res
```

## Define the activities

``` r
tech <- rbind(
  land         = rep(1,5),
  labor        = c(20,25,15,30,10),
  nitrogen     = c(80,100,0,30,0),
  oat_min      = c(1,0,0,0,0),  oat_max = c(1,0,0,0,0),
  barley_min   = c(0,1,0,0,0),  barley_max = c(0,1,0,0,0),
  lupin_min    = c(0,0,1,0,0),  lupin_max = c(0,0,1,0,0),
  fava_min     = c(0,0,0,1,0),  fava_max = c(0,0,0,1,0),
  pasture_min  = c(0,0,0,0,1),  pasture_max = c(0,0,0,0,1)
)
colnames(tech) <- c("oat","barley","lupin","fava","pasture")

acts <- define_activities(
  activities = colnames(tech),
  activity_requirements_matrix = tech,
  objective = c(oat=400, barley=450, lupin=350, fava=500, pasture=360)
)
```

## Build and solve the model

``` r
model   <- create_ram_model(res, acts)
solution <- solve_ram(model, direction = "max")
solution
```

## Inspect results

``` r
print(solution$optimal_activities)
print(solution$objective_value)
print(sensitivity_ram(solution))
summary_ram(solution)
```

## Plot the solution

``` r
plot_ram(solution)
```

------------------------------------------------------------------------

# Package Vignettes

For a practical demonstration, run:

``` r
vignette("WinterCropsAreaBounds", package = "ram")
```

------------------------------------------------------------------------

# Contributing

Contributions welcome — please [open
issues](https://github.com/PAS-AGRO-PAS/ram/issues) or submit [pull
requests](https://github.com/PAS-AGRO-PAS/ram/pulls).

------------------------------------------------------------------------

# License

Licensed under MIT — see
[LICENSE](https://github.com/PAS-AGRO-PAS/ram/blob/main/LICENSE.md).
