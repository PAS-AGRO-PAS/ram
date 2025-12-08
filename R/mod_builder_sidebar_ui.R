#' builder_sidebar UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @importFrom shiny NS tagList textInput numericInput selectInput uiOutput
#' @importFrom shiny downloadButton actionButton tags h4
#' @noRd 
mod_builder_sidebar_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h4("Resources"),
    textInput( ns("res_name"),  "Name",          ""           ),
    numericInput(ns("res_avail"), "Availability", 0, min = 0),
    selectInput( ns("res_dir"),   "Direction",    c("<=", ">=")),
    fluidRow(
      column(
        width = 6,
        actionButton(ns("add_res"), "+ Add", width = "100%")
      ),
      column(
        width = 6,
        actionButton(ns("del_res"), "Remove", width = "100%")
      )
    ),
    tags$hr(),
    fluidRow(
      column(
        width = 6,
        downloadButton(ns("download_res_csv"),  "Export CSV",  width = "100%")
      ),
      column(
        width = 6,
        downloadButton(ns("download_res_xlsx"), "Export XLSX", width = "100%")
      )
    ),
    tags$hr(),
    h4("Activities"),
    textInput( ns("act_name"), "Name",            ""  ),
    numericInput(ns("act_obj"), "Objective (EUR/unit)",0),
    uiOutput(   ns("coef_inputs") ),    # dynamic UI
    fluidRow(
      column(
        width = 6,
        actionButton(ns("add_act"),  "+ Add",   width = "100%")
      ),
      column(
        width = 6,
        actionButton(ns("del_act"),  "Remove",width = "100%")
      )
    ),
    tags$hr(),
    fluidRow(
      column(
        width = 6,
        downloadButton(ns("download_act_csv"),  "Export CSV",  width = "100%")
      ),
      column(
        width = 6,
        downloadButton(ns("download_act_xlsx"), "Export XLSX", width = "100%")
      )
    ),
    tags$hr(),
    actionButton(ns("handoff_solver"), "Open In Solver", icon = icon("share-from-square"), width = "100%")
  )
}
