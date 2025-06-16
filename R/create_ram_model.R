#' Create a RAM Model
#'
#' Combines resources and activities into a RAM model object.
#'
#' @param resources Data frame from \code{define_resources()}.
#' @param activities Data frame from \code{define_activities()}.
#' @return List of class 'ram_model'.
#' @export
create_ram_model <- function(resources, activities) {
  model <- list(resources = resources, activities = activities)
  class(model) <- "ram_model"
  model
}
