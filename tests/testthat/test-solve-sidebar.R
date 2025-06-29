# tests/testthat/test-solve-sidebar.R

#' @import shiny
#' @import DT
library(testthat)
library(shiny)
library(DT)
dummy_proxy <- structure(list(), class = "dataTableProxy")

test_that("mod_solve_sidebar_server solves correctly", {
  dummy_proxy <- structure(list(), class = "dataTableProxy")
  
  testthat::with_mock(
    `DT::replaceData` = function(...) NULL,  # mock da função problemática
    {
      testServer(mod_solve_sidebar_server, args = list(
        id = "solve",
        rv = reactiveValues(
          resources = data.frame(resource="r1", availability=1, direction="<=", stringsAsFactors=FALSE),
          activities = data.frame(activity="a1", r1=1, objective=5, stringsAsFactors=FALSE),
          solution = NULL
        ),
        resources_proxy = dummy_proxy,
        activities_proxy = dummy_proxy
      ), {
        tmp_r <- tempfile(fileext = ".csv")
        write.csv(data.frame(resource="r1", availability=2, direction="<="), tmp_r, row.names = FALSE)
        session$setInputs(resource_file = list(datapath = tmp_r))
        session$flushReact()
        expect_equal(rv$resources$availability, 2)
        
        tmp_a <- tempfile(fileext = ".csv")
        write.csv(data.frame(activity="a1", r1=1, objective=5), tmp_a, row.names = FALSE)
        session$setInputs(activity_file = list(datapath = tmp_a))
        session$flushReact()
        expect_true(!is.null(rv$activities))
        
        session$setInputs(direction = "max")
        session$setInputs(solve = 1)
        session$flushReact()
        expect_true(!is.null(rv$solution))
        expect_named(rv$solution$optimal_activities, "a1")
      })
    }
  )
})
