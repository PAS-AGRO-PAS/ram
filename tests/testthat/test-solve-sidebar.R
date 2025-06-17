# tests/testthat/test-solve-sidebar.R

#' @import shiny
#' @import DT
library(testthat)
library(shiny)
library(DT)
testServer(
  mod_solve_sidebar_server,
  args = list(
    id = "solve",
    rv = reactiveValues(
      resources = data.frame(resource="r1", availability=1, direction="<=", stringsAsFactors=FALSE),
      activities = data.frame(activity="a1", r1=1, objective=5, stringsAsFactors=FALSE),
      solution = NULL
    )
  ),
  {
    # Simulate upload of resources
    tmp_r <- tempfile(fileext = ".csv")
    write.csv(data.frame(resource="r1", availability=2, direction="<="), tmp_r, row.names = FALSE)
    session$setInputs(resource_file = list(datapath = tmp_r))
    expect_equal(rv$resources$availability, 2)
    
    # Simulate upload of activities
    tmp_a <- tempfile(fileext = ".csv")
    write.csv(data.frame(activity="a1", r1=1, objective=5), tmp_a, row.names = FALSE)
    session$setInputs(activity_file = list(datapath = tmp_a))
    expect_true(!is.null(rv$activities))
    
    # Trigger solve and expect a solution
    session$setInputs(direction = "max")
    session$setInputs(solve = 1)
    session$flushReact()  # ensure observers are run :contentReference[oaicite:3]{index=3}
    expect_true(!is.null(rv$solution))
    expect_named(rv$solution$optimal_activities, "a1")
  }
)
