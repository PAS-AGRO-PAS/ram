#' Define Activities
#'
#' Creates a data frame describing each activity, using resource names for columns.
#'
#' @param activities Character vector of activity names.
#' @param activity_requirements_matrix Numeric matrix (resources x activities) of resource usage per activity. Row names must be resource names.
#' @param objective Numeric vector of objective function coefficient per activity (e.g., profit for maximization, cost for minimization).
#' @return Data frame with one row per activity, columns for each resource (named), and an objective column.
#' @export
define_activities <- function(activities, activity_requirements_matrix, objective) {
  stopifnot(length(activities) == ncol(activity_requirements_matrix))
  stopifnot(length(objective) == length(activities))
  if (is.null(rownames(activity_requirements_matrix))) {
    resource_names <- paste0("resource_", seq_len(nrow(activity_requirements_matrix)))
    warning("activity_requirements_matrix has no row names: using generic resource names.")
  } else {
    resource_names <- rownames(activity_requirements_matrix)
  }
  df <- as.data.frame(t(activity_requirements_matrix))
  colnames(df) <- resource_names
  df$activity <- activities
  df$objective <- objective
  df <- df[, c("activity", resource_names, "objective")]
  rownames(df) <- activities
  df
}
