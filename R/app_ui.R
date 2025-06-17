# R/app_ui.R

#' The application User-Interface
#'
#' @import bslib shiny DT plotly
#' @noRd
library(bslib)
library(shiny)
library(DT)
library(plotly)

app_ui <- function(request) {
  mode <- golem::get_golem_options("app_mode")
  if (is.null(mode)) mode <- "solve"
  
  # Temporarily rename or remove `_brand.yml`
  # or in app_ui.R:
  theme = bs_theme(
    version   = 5, 
    preset    = "bootstrap", brand = FALSE,
    bg        = "#ffffff", fg = "#432918",
    primary   = "#A3BF4E", secondary = "#CBBBA0",
    success   = "#28a745", info      = "#17a2b8",
    warning   = "#ffc107", danger    = "#dc3545",
    light     = "#f8f9fa", dark      = "#A3897B"
  )
  
  fluidPage(
    theme = theme,
    title = uiOutput("app_logo"),  # Dynamic logo + app name
    titlePanel(
      "PAS‑AGRO‑PAS – ram app",
      windowTitle = "PAS‑AGRO‑PAS – ram app"
    ),
    sidebarLayout(
      sidebarPanel(
        width = 3,
        input_dark_mode(id = "color_mode"),  # Light/dark toggle
        if (mode == "solve") {
          mod_solve_sidebar_ui("solve")
        } else {
          mod_builder_sidebar_ui("builder")
        }
      ),
      mainPanel(
        do.call(
          tabsetPanel,
          c(
            list(id = "main_tabs"),
            if (mode == "solve") solve_panels() else builder_panels()
          )
        )
      )
    )
  )
}

# Panels for "solve" mode
solve_panels <- function() {
  list(
    tabPanel("About",
             h2("How to use the ram app"),
             HTML('
        <ol>
          <li><b>Templates:</b> Download, upload or edit your CSV templates.</li>
          <li><b>Tables:</b> Inspect & edit loaded tables.</li>
          <li><b>Solve:</b> Choose objective and click <em>Solve Model</em>.</li>
          <li><b>Results:</b> View optimal solution, plot, and sensitivity.</li>
          <li><b>Builder:</b> Build custom resources & activities.</li>
        </ol>
        <hr><h2>Acknowledgements</h2>
      ')
    ),
    tabPanel("Tables",
             tabsetPanel(
               type = "tabs",
               tabPanel("Resources",
                        h4("Resource Constraints"),
                        DTOutput("resources_table"),
                        downloadButton("download_resources_csv", "Download CSV")
               ),
               tabPanel("Activities",
                        h4("Activity Definitions"),
                        DTOutput("activities_table"),
                        downloadButton("download_activities_csv", "Download CSV")
               )
             )
    ),
    tabPanel("Optimization",
             h2("Optimal Solution"),
             DTOutput("solution_tbl"),
             verbatimTextOutput("objective_val"),
             plotlyOutput("activity_plot")
    ),
    tabPanel("Sensitivity",
             h2("Sensitivity Analysis"),
             DTOutput("sensitivity_tbl")
    )
  )
}

# Panels for "builder" mode
builder_panels <- function() {
  list(
    tabPanel("Interactive Builder",
 #            h2("Interactive Builder"),
             p("Use the sidebar to build & export your tables."),
             h4("Resources Table"), DTOutput("res_tbl"),
             h4("Activities Table"), DTOutput("act_tbl")
    )
  )
}
