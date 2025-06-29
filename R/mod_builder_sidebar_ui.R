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
    actionButton(ns("add_res"),   "➕ Add Resource",   width="70%"),
    actionButton(ns("del_res"),   "🗑 Remove Resource",width="70%"),
    tags$hr(),
    downloadButton(ns("download_res_csv"),  "Export Resources CSV",  width="70%"),
    downloadButton(ns("download_res_xlsx"), "Export Resources XLSX", width="70%"),
    tags$hr(),
    h4("Activities"),
    textInput( ns("act_name"), "Name",            ""  ),
    numericInput(ns("act_obj"), "Objective €/unit",0),
    uiOutput(   ns("coef_inputs") ),    # dynamic UI
    actionButton(ns("add_act"),  "➕ Add Activity",   width="70%"),
    actionButton(ns("del_act"),  "🗑 Remove Activity",width="70%"),
    tags$hr(),
    downloadButton(ns("download_act_csv"),  "Export Activities CSV",  width="70%"),
    downloadButton(ns("download_act_xlsx"), "Export Activities XLSX", width="70%")
  )
}
