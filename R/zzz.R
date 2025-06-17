# R/external_resources.R

#' @noRd
.onLoad <- function(...) {
  # Map the URL prefix "/www" to your app's www folder inside inst/app
  shiny::addResourcePath(
    "www",
    system.file("app/www", package = "ram")
  )
}

#' Add golem's external resources
#' @noRd
#' @import shiny
#' @importFrom golem add_resource_path bundle_resources activate_js favicon
golem_add_external_resources <- function(){
  # Also map "/img" to the same folder if you'd like both URL schemes
  golem::add_resource_path("img", app_sys("app/www"))
  
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "ram"
    )
  )
}

#' @title Get Path to Extdata File
#' @description
#' Returns the installed path to a file bundled in `inst/extdata`.
#'
#' @param filename Name of file under inst/extdata
#' @param mustWork Logical; if TRUE throws error if not found
#' @return Full path to the installed extdata file
#' @export
get_extdata <- function(filename, mustWork = TRUE) {
  system.file("extdata",
              filename, package = "ram",
              mustWork = mustWork)
}

# Avoid conflicts with shiny's versions of dataTableOutput/renderDataTable
#' @rawNamespace import(shiny, except = c(dataTableOutput, renderDataTable))
NULL
