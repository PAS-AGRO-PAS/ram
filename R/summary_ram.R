#' Summary Table for RAM Model Solution (DT version)
#'
#' Returns a DT table summarizing the solution of a solved RAM model.
#'
#' @param solution List returned by \code{solve_ram()}.
#' @importFrom DT DTOutput renderDT datatable
#' @return DT datatable with optimal activity levels and objective value.
#' @export
summary_ram <- function(solution) {
  # Ensure names and values
  activities <- as.numeric(solution$optimal_activities)
  activity_names <- names(solution$optimal_activities)
  if (is.null(activity_names)) activity_names <- paste0("activity_", seq_along(activities))
  df <- data.frame(
    Activity = activity_names,
    Level = activities,
    stringsAsFactors = FALSE
  )
  # Add objective value as a separate row (optional)
  df <- rbind(
    df,
    data.frame(Activity = "Objective value", Level = solution$objective_value)
  )
  # Return as a DT datatable
  DT::datatable(df, rownames = FALSE,
                options = list(dom = 't', pageLength = nrow(df)))
}
