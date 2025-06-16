#' Plot RAM Model Solution (plotly version)
#'
#' Visualizes the optimal allocation (activities) as an interactive barplot.
#'
#' @param solution List returned by \code{solve_ram()}.
#' @importFrom plotly plot_ly layout
#' @importFrom dplyr %>%
#' @export
plot_ram <- function(solution) {
  # Defensive: allow either vector or length-1 data.frame
  acts <- as.numeric(solution$optimal_activities)
  names(acts) <- names(solution$optimal_activities)
  plotly::plot_ly(
    x = names(acts),
    y = acts,
    type = "bar",
    marker = list(color = "skyblue"),
    hoverinfo = "x+y",
    name = "Level"
  ) %>%
    plotly::layout(
      title = "Optimal Resource Allocation",
      xaxis = list(title = "Activity"),
      yaxis = list(title = "Level")
    )
}
