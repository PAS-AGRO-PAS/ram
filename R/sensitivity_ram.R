#' Sensitivity Analysis for RAM (from solution object)
#'
#' @param solution List returned by solve_ram().
#' @return Data frame: resource, direction, availability, shadow_price, lower/upper bounds
#' @export
sensitivity_ram <- function(solution) {
  res      <- solution$lp_result
  resources <- solution$model_resources
  n_res   <- nrow(resources)
  duals   <- res$duals[seq_len(n_res)]
  lower   <- res$duals.from[seq_len(n_res)]
  upper   <- res$duals.to[seq_len(n_res)]
  data.frame(
    resource     = resources$resource,
    direction    = resources$direction,
    availability = resources$availability,
    shadow_price = duals,
    lower_bound  = lower,
    upper_bound  = upper,
    stringsAsFactors = FALSE
  )
}
