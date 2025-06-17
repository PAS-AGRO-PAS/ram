# R/run_apps.R

#' Launch the RAM app in solver mode
#' @param ... Additional golem options passed to the app (via `get_golem_options()`)
#' @export
run_app_solve <- function(...) {
  with_golem_options(
    app = shiny::shinyApp(ui = app_ui, server = app_server),
    golem_opts = c(list(app_mode = "solve"), list(...))
  )
}

#' Launch the RAM app in builder mode
#' @param ... Additional golem options passed to the app
#' @export
run_app_builder <- function(...) {
  with_golem_options(
    app = shiny::shinyApp(ui = app_ui, server = app_server),
    golem_opts = c(list(app_mode = "builder"), list(...))
  )
}

#' Launch the RAM app in specified mode ("solve" or "builder")
#' @param mode Character, one of "solve" or "builder" (default: "solve")
#' @param ... Additional golem options
#' @export
run_app <- function(mode = c("solve", "builder"), ...) {
  mode <- match.arg(mode)
  with_golem_options(
    app = shiny::shinyApp(ui = app_ui, server = app_server),
    golem_opts = c(list(app_mode = mode), list(...))
  )
}
