#' Optimize a Resource Allocation Model
#'
#' Solves a linear programming model for resource allocation in agro-pastoral systems.
#'
#' @param profit Numeric vector. Profit (or objective coefficient) for each activity.
#' @param resource_use Matrix. Rows = resources, Columns = activities. Each entry gives the use of resource per activity unit.
#' @param availability Numeric vector. Total available amount for each resource (must match number of rows in \code{resource_use}).
#' @importFrom lpSolve lp
#' @importFrom stats  setNames
#' @importFrom config get
#' @return A list with:
#'   \item{optimal_activities}{Named vector of optimal activity levels}
#'   \item{max_profit}{Optimal value of the objective function}
#'   \item{status}{Status code from lpSolve (0 = success)}
#' @examples
#' profit <- c(20, 25)
#' resource_use <- matrix(c(1, 2, 2, 3, 3, 0), nrow = 3, byrow = TRUE)
#' rownames(resource_use) <- c("land", "labor", "feed")
#' colnames(resource_use) <- c("grazing", "haymaking")
#' availability <- c(100, 200, 300)
#' names(availability) <- rownames(resource_use)
#' ram_optimize(profit, resource_use, availability)
#' @export
ram_optimize <- function(profit, resource_use, availability) {
  stopifnot(length(availability) == nrow(resource_use))
  stopifnot(length(profit) == ncol(resource_use))
  if (!requireNamespace("lpSolve", quietly = TRUE)) {
    stop("Package 'lpSolve' is required. Please install it.")
  }
  res <- lpSolve::lp(
    direction = "max",
    objective.in = profit,
    const.mat = resource_use,
    const.dir = rep("<=", length(availability)),
    const.rhs = availability
  )
  list(
    optimal_activities = setNames(res$solution, colnames(resource_use)),
    max_profit = res$objval,
    status = res$status
  )
}
