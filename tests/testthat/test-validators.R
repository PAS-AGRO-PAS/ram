test_that("validate_resource_upload accepts well-formed data", {
  df <- data.frame(
    resource = c("land", "labor"),
    availability = c(100, 200),
    direction = c("<=", ">="),
    stringsAsFactors = FALSE
  )
  result <- ram:::validate_resource_upload(df)
  expect_equal(nrow(result), 2)
})

test_that("validate_resource_upload rejects missing columns", {
  df <- data.frame(
    resource = "land",
    availability = 100
  )
  expect_error(ram:::validate_resource_upload(df))
})

test_that("validate_resource_upload rejects duplicate resources", {
  df <- data.frame(
    resource = c("land", "land"),
    availability = c(1, 2),
    direction = c("<=", "<="),
    stringsAsFactors = FALSE
  )
  expect_error(ram:::validate_resource_upload(df))
})

test_that("validate_activity_upload enforces numeric columns", {
  df <- data.frame(
    activity = c("a", "b"),
    resource1 = c(1, 2),
    objective = c(10, 20),
    stringsAsFactors = FALSE
  )
  expect_no_error(ram:::validate_activity_upload(df))
  
  df_bad <- data.frame(
    activity = "a",
    resource1 = "x",
    objective = 10,
    stringsAsFactors = FALSE
  )
  expect_error(ram:::validate_activity_upload(df_bad))
})

test_that("validate_activity_upload rejects duplicate activities", {
  df <- data.frame(
    activity = c("a", "a"),
    resource1 = c(1, 2),
    objective = c(10, 20),
    stringsAsFactors = FALSE
  )
  expect_error(ram:::validate_activity_upload(df))
})

test_that("ensure_builder_activity_columns maintains required structure", {
  df <- data.frame(
    Name = "act1",
    Objective = 10,
    stringsAsFactors = FALSE
  )
  updated <- ram:::ensure_builder_activity_columns(df, c("land", "labor"))
  expect_true(all(c("land", "labor") %in% names(updated)))
  expect_true(all(updated$land == 0))
})

test_that("ensure_builder_activity_columns works with empty activity table", {
  df <- data.frame(Name = character(), Objective = numeric(), stringsAsFactors = FALSE)
  out <- ram:::ensure_builder_activity_columns(df, c("land"))
  expect_equal(nrow(out), 0)
  expect_true("land" %in% names(out))
})

test_that("ensure_builder_activity_columns fills base columns when missing", {
  df <- data.frame(stringsAsFactors = FALSE)
  out <- ram:::ensure_builder_activity_columns(df, c("land"))
  expect_equal(names(out), c("Name", "Objective", "land"))
  expect_equal(nrow(out), 0)
})

test_that("ensure_builder_activity_columns ignores empty or NA resource names", {
  df <- data.frame(Name = character(), Objective = numeric(), stringsAsFactors = FALSE)
  out <- ram:::ensure_builder_activity_columns(df, c("land", "", NA, "land"))
  expect_equal(names(out), c("Name", "Objective", "land"))
})

test_that("parse_builder_payload accepts base64-encoded payloads", {
  payload <- list(
    resources = data.frame(
      resource = "land",
      availability = 10,
      direction = "<=",
      stringsAsFactors = FALSE
    ),
    activities = data.frame(
      activity = "a1",
      land = 1,
      objective = 5,
      stringsAsFactors = FALSE
    )
  )
  json_txt <- jsonlite::toJSON(payload, dataframe = "rows", auto_unbox = TRUE, digits = NA)
  b64 <- jsonlite::base64_enc(json_txt)
  url <- paste0("?mode=solve&builder_payload=", URLencode(b64, reserved = TRUE))
  parsed <- ram:::parse_builder_payload(url)
  expect_true(!is.null(parsed))
  expect_equal(parsed$resources$resource, "land")
  expect_equal(parsed$activities$activity, "a1")
})
