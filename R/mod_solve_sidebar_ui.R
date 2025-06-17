#' Solve Sidebar UI Module
#'
#' @noRd
#' @importFrom shiny NS tagList downloadButton fileInput radioButtons actionButton tags h4
mod_solve_sidebar_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h4("Templates & Upload"),
    downloadButton(ns("download_resource_template"), "Download Resource Template", width = "80%"),
    fileInput(ns("resource_file"), "Upload Resource CSV", accept = ".csv"),
    tags$hr(),
    downloadButton(ns("download_activity_template"), "Download Activity Template", width = "80%"),
    fileInput(ns("activity_file"), "Upload Activity CSV", accept = ".csv"),
    tags$hr(),
    h4("Solve Settings"),
    radioButtons(ns("direction"), "Objective:",
                 choices = c("Maximize" = "max", "Minimize" = "min"),
                 selected = "max", inline = TRUE),
    actionButton(ns("solve"), "Solve Model", icon = icon("play"),
                 class = "btn-primary", width = "80%"),
    tags$hr()
  )
}
