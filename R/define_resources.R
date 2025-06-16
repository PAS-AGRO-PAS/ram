#' Define Resource Constraints
#'
#' Creates a data frame of resource constraints for the RAM model,
#' allowing both upper (`"<=``) and lower (`">="`) bounds.
#'
#' @param resources Character vector of constraint/resource names (e.g., "land", "labor", "protein_min").
#' @param availability Numeric vector of right-hand sides (amounts available or required).
#' @param direction Character vector of constraint directions; each value must be either "<=" or ">=".
#'
#' @return Data frame with columns: `resource`, `availability`, and `direction`.
#' @examples
#' # Upper bounds only
#' define_resources(
#'   resources = c("land", "labor"),
#'   availability = c(100, 200),
#'   direction = c("<=", "<=")
#' )
#'
#' # Mixed upper/lower bounds (min requirement for protein, max for feed)
#' define_resources(
#'   resources = c("protein_min", "feed_max"),
#'   availability = c(2.5, 10),
#'   direction = c(">=", "<=")
#' )
#' @export
define_resources <- function(resources, availability, direction) {
  stopifnot(
    length(resources) == length(availability),
    length(resources) == length(direction),
    all(direction %in% c("<=", ">="))
  )
  data.frame(
    resource = resources,
    availability = availability,
    direction = direction,
    stringsAsFactors = FALSE
  )
}
