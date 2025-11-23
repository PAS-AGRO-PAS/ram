# R/app_ui.R

#' The application User-Interface
#'
#' @importFrom shiny fluidPage titlePanel sidebarLayout sidebarPanel mainPanel tabsetPanel uiOutput
#' @importFrom bslib input_dark_mode bs_theme
#' @importFrom DT DTOutput
#' @importFrom plotly plotlyOutput
#' @importFrom golem get_golem_options
app_ui <- function(request) {
  mode_opt <- golem::get_golem_options("app_mode")
  query_mode <- NULL
  if (!is.null(request)) {
    query <- shiny::parseQueryString(request$QUERY_STRING)
    query_mode <- query$mode
  }
  mode <- query_mode
  if (is.null(mode)) mode <- mode_opt
  if (is.null(mode) || !(mode %in% c("solve", "builder"))) mode <- "solve"
  
  theme <- bs_theme(
    version   = 5,
    preset    = "bootstrap",
    brand     = FALSE,
    bg        = "#ffffff", fg        = "#432918",
    primary   = "#A3BF4E", secondary = "#CBBBA0",
    success   = "#28a745", info      = "#17a2b8",
    warning   = "#ffc107", danger    = "#dc3545",
    light     = "#f8f9fa", dark      = "#A3897B"
  )
  
  fluidPage(
    theme = theme,
    titlePanel(
      title = uiOutput("app_logo"),
      windowTitle = "ram app"
    ),

    sidebarLayout(
      sidebarPanel(
        width = 3,
        input_dark_mode(id = "color_mode"),
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
    ),
    tags$head(
      tags$script(
        HTML("
          Shiny.addCustomMessageHandler('ram-open-solver', function(message) {
            if (!message || !message.payload) return;
            var payload = encodeURIComponent(message.payload);
            var baseUrl = window.location.protocol + '//' + window.location.host + window.location.pathname;
            var target = baseUrl + '?mode=solve&builder_payload=' + payload;
            window.open(target, '_blank');
          });
        ")
      )
    )
  )
}

solve_panels <- function() {
  list(
    tabPanel("About",
             h2("How to use the ram app - Solver Mode"),
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
                        DTOutput("activities_table"),
                        downloadButton("download_activities_csv", "Download CSV")
               )
             )
    ),
    tabPanel("Optimization",
             h2("Optimal Solution"),
             uiOutput("solver_status"),
             div(
               class = "d-flex gap-2 flex-wrap mb-3",
               downloadButton("download_lp_report", "Download Solver Report (.rds)")
             ),
             DT::DTOutput("solution_tbl"),
             verbatimTextOutput("objective_val"),
             plotly::plotlyOutput("activity_plot"),
             h4("Solver Diagnostics"),
             verbatimTextOutput("solver_diagnostics")
    ),
    tabPanel("Sensitivity",
             h2("Sensitivity Analysis"),
             DT::DTOutput("sensitivity_tbl")
    )
  )
}

builder_panels <- function() {
  list(
    tabPanel("About",
             h2("How to use the ram app - Builder Mode"),
             HTML('
             <p>
                <b>Step 1:</b> <i>Use the sidebar to define your resource constraints</i>. You can start from a blank table or edit pre-loaded examples.<br>
                <b>Step 2:</b> <i>Add your activities</i>, specifying how each one uses resources (e.g., crops, feeds) and their associated objective values.<br>
                <b>Step 3:</b> <i>Once both tables are complete</i>, you can export them for later use in the solver app.<br>
             </p>
             <p>
                <b>Resources Table:</b> Each row represents a constraint (e.g., land ≤ 100 ha, labor ≤ 200 hours, nitrogen = 350 Kg).<br>
                <b>Activities Table:</b> Each row defines an activity, with the corresponding use of each resource and a target value (e.g., cost or profit).<br>
             </p>
             <p>
                <b>Need help?</b> Use the template options in the sidebar to load example data or reset the tables.
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
    tabPanel("Interactive Builder",
             p("Use the sidebar to build & export your tables."),
             h4("Resources Table"),  DT::DTOutput("res_tbl"),
             h4("Activities Table"), DT::DTOutput("act_tbl")
    )
  )
}
