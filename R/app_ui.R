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
                 <p>
                    <b>Step 1:</b> <i>Enter your resource constraints</i> in the left panel. You can use the sample template, upload your own CSV, or edit the table directly.<br>
                    <b>Step 2:</b> <i>Enter your activity/resource matrix and objective values</i> (e.g., crop profit, feed cost). Again, upload a CSV or edit the table.<br>
                    <b>Step 3:</b> <i>Choose whether to maximize or minimize</i> (e.g., maximize profit or minimize cost).<br>
                    <b>Step 4:</b> Click <b>Solve Model</b>.<br>
                    <b>Step 5:</b> Results appear in the Optimization Results tab.
                 </p>
                 <p>
                    <b>Resources Table:</b> Each row defines a constraint (e.g., land &le; 100, protein &ge; 2.5).<br>
                    <b>Activities Table:</b> Each row is an activity (e.g., a crop or a feed) with columns for resources required.<br>
                 </p>
                 <p>
                    <b>Need help?</b> Download example templates from the left panel.
                 </p>
                 <hr>
                    <h3>Acknowledgements</h3>
                <div style="display: flex; align-items: center; margin-bottom:10px;">
                  <img src="www/eu.jpg" height="70px" style="margin-right:15px;">
                  <img src="www/prima.png" height="70px" style="margin-right:15px;">
                  <img src="www/fct.png" height="100px" style="margin-right:15px; background:#000;padding:2px 5px; border-radius:5px;">
                  <img src="www/pasagropas.png" height="150px" style="margin-right:15px;">
                </div>
                <p style="font-size:15px;">
                   This research was carried out within the framework of the PAS-AGRO-PAS project. The PAS-AGRO-PAS is part of the PRIMA program supported by the European Union.
                   This project received funding from the Fundação para a Ciência e Tecnologia (FCT), and other National Funding Agencies as part of the PRIMA program.
                 </p>
                 ')
    ),
    tabPanel("Tables",
             tabsetPanel(
               type = "tabs",
               tabPanel("Resources",
                        h4("Resource Constraints"),
                        DT::DTOutput("resources_table"),
                        downloadButton("download_resources_csv", "Download CSV")
               ),
               tabPanel("Activities",
                        h4("Activity Definitions"),
                        DT::DTOutput("activities_table"),
                        downloadButton("download_activities_csv", "Download CSV")
               )
             )
    ),
    tabPanel("Optimization",
             h2("Optimal Solution"),
             DT::DTOutput("solution_tbl"),
             verbatimTextOutput("objective_val"),
             plotlyOutput("activity_plot")
    ),
    tabPanel("Sensitivity",
             h2("Sensitivity Analysis"),
             DT::DTOutput("sensitivity_tbl")
    )
  )
}

# Panels for "builder" mode
builder_panels <- function() {
  list(
    tabPanel("Interactive Builder",
 #           h2("Interactive Builder"),
             p("Use the sidebar to build & export your tables."),
             h4("Resources Table"), DT::DTOutput("res_tbl"),
             h4("Activities Table"), DT::DTOutput("act_tbl")
    )
  )
}
