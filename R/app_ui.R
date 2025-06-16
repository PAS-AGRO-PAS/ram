# R/app_ui.R

#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom DT DTOutput
#' @import plotly
#' @noRd
app_ui <- function(request) {
  tagList(
    golem_add_external_resources(),

    fluidPage(
      titlePanel("PAS-AGRO-PAS - ram app"),

      sidebarLayout(
        sidebarPanel(
          width = 3,
          h4("Templates & Upload"),
          downloadButton("download_resource_template", "Resource Template", width = "100%"),
          fileInput("resource_file",  "Upload Resource CSV",  accept = c(".csv")),
          tags$hr(),

          downloadButton("download_activity_template", "Activity Template", width = "100%"),
          fileInput("activity_file",  "Upload Activity CSV",  accept = c(".csv")),
          tags$hr(),

          h4("Add / Remove Rows"),
          fluidRow(
            column(6, actionButton("add_resource",  "Add Resource",  width = "100%")),
            column(6, actionButton("del_resource",  "Remove Resource", width = "100%"))
          ),
          tags$br(),
          fluidRow(
            column(6, actionButton("add_activity",  "Add Activity",  width = "100%")),
            column(6, actionButton("del_activity",  "Remove Activity", width = "100%"))
          ),
          tags$hr(),

          h4("Solve Settings"),
          radioButtons("direction", "Objective:",
                       choices = c("Maximize" = "max", "Minimize" = "min"),
                       selected = "max", inline = TRUE),
          actionButton("solve", "Solve Model", icon = icon("play"),
                       class = "btn-primary", width = "60%")
        ),

        mainPanel(
          tabsetPanel(
            tabPanel("About",
                     h2("How to use the ram app"),
                     HTML('
                <ol>
                  <li><b>Templates:</b> Download, edit/upload, or fill in the tables.</li>
                  <li><b>Tables:</b> Edit your constraints (Resources) and activities (resource usage + margin).</li>
                  <li><b>Solve:</b> Choose maximize/minimize, then click <em>Solve Model</em>.</li>
                  <li><b>Results:</b> Inspect the Optimal Levels, Objective, Plot, and Sensitivity.</li>
                </ol>
                <hr>
                <h2>Acknowledgements</h2>
                <div style="display:flex; gap:15px; align-items:center;">
                  <img src="img/eu.jpg" height="110px"/>
                  <img src="img/prima.png" height="110px"/>
                  <img src="img/fct.jpg" height="150px"/>
                  <img src="img/pasagropas.png" height="150px"/>
                </div>
                <p style="font-size:1.2em;">
                  This work was carried out within the PAS-AGRO-PAS project (PRIMA, EU & FCT funding).
                </p>
              ')
            ),

            tabPanel("Tables",
                     tabsetPanel(type = "tabs",
                                 tabPanel("Resources",
                                          h4("Resources Constraints"),
                                          # new download button for current table
                                          downloadButton("download_resources_csv", "Download Table", width = "30%"),
                                          tags$br(), tags$br(),
                                          DTOutput("resources_table")
                                 ),
                                 tabPanel("Activities",
                                          h4("Activities Definition"),
                                          # new download button for current table
                                          downloadButton("download_activities_csv", "Download Table", width = "30%"),
                                          tags$br(), tags$br(),
                                          DTOutput("activities_table")
                                 )
                     )
            ),

            tabPanel("Optimization",
                     h2("Optimal Solution"),
                     h4("Optimal Activity Levels"),
                     DTOutput("solution_tbl"),
                     h4("Objective Value"),
                     verbatimTextOutput("objective_val"),
                     h4("Activity Plot"),
                     plotlyOutput("activity_plot")
            ),

            tabPanel("Sensitivity",
                     h2("Sensitivity Analysis"),
                     DTOutput("sensitivity_tbl")
            )
          )  # end main tabset
        )    # end mainPanel
      )      # end sidebarLayout
    )        # end fluidPage
  )          # end tagList
}
