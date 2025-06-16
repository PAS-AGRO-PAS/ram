test_that("define_resources supports directions", {
  # Upper bounds only
  res1 <- define_resources(c("land", "labor"), c(100, 200), c("<=", "<="))
  expect_equal(nrow(res1), 2)
  expect_equal(res1$direction[1], "<=")

  # Mixed constraints
  res2 <- define_resources(
    c("protein_min", "feed_max"),
    c(2.5, 10),
    c(">=", "<=")
  )
  expect_equal(res2$direction[1], ">=")
  expect_equal(res2$availability[2], 10)
})

test_that("define_activities works as expected", {
  acts <- define_activities(
    activities = c("wheat", "maize"),
    activity_requirements_matrix = matrix(
      c(1, 1, 3, 2), nrow = 2, byrow = TRUE,
      dimnames = list(c("land", "labor"), NULL)
    ),
    objective = c(1000, 800)
  )
  expect_equal(nrow(acts), 2)
  expect_equal(acts$objective[1], 1000)
  expect_equal(acts$activity[2], "maize")
})

test_that("RAM model solves with mixed constraints", {
  resources <- define_resources(
    c("land", "labor_min", "labor_max"),
    c(100, 80, 200),
    c("<=", ">=", "<=")
  )
  resource_matrix <- matrix(
    c(1, 1,   # land per wheat, maize
      3, 2,   # labor per wheat, maize
      3, 2),  # same as above (for labor_min and labor_max)
    nrow = 3, byrow = TRUE,
    dimnames = list(c("land", "labor_min", "labor_max"), NULL)
  )
  acts <- define_activities(
    activities = c("wheat", "maize"),
    activity_requirements_matrix = resource_matrix,
    objective = c(1000, 800)
  )
  model <- create_ram_model(resources, acts)
  solution <- solve_ram(model)
  expect_true(is.list(solution))
  expect_true(solution$objective_value > 0)
})
