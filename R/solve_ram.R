#' Solve a Resource Allocation Model with Mixed Constraints
#'
#' @param model A 'ram_model' object as returned by \code{create_ram_model()}.
#' @param direction "max" to maximize the objective, "min" to minimize (default: "max").
#' @importFrom lpSolve lp
#' @importFrom stats  setNames
#' @importFrom config get
#' @return A list with:
#'   \item{optimal_activities}{Named vector of optimal activity levels}
#'   \item{objective_value}{Optimal value of the objective function}
#'   \item{status}{Status code from lpSolve (0 = success)}
#' @export
solve_ram <- function(model, direction = "max") {
  if (!requireNamespace("lpSolve", quietly = TRUE)) stop("Package 'lpSolve' is required. Please install it.")
  activity_df <- model$activities
  resource_cols <- setdiff(names(activity_df), c("activity", "objective"))
  resource_use <- as.matrix(activity_df[, resource_cols, drop = FALSE])
  objective <- activity_df$objective
  resource_use <- t(resource_use)
  availability <- model$resources$availability
  constraint_direction <- model$resources$direction

  res <- lpSolve::lp(
    direction = direction,
    objective.in = objective,
    const.mat = resource_use,
    const.dir = constraint_direction,
    const.rhs = availability
  )

  # Save the whole lpSolve result in solution object
  list(
    optimal_activities = setNames(res$solution, activity_df$activity),
    objective_value    = res$objval,
    status             = res$status,
    lp_result          = res,   # <-- Store everything!
    model_resources    = model$resources # for resource names
  )
}
