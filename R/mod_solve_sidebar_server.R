# R/mod_solve_sidebar_server.R

#' Solve Sidebar Server Module
#'
#' @description Server logic for the “solve” sidebar: download templates,
#'   upload CSVs, and trigger the solver.
#' @param id Module namespace ID
#' @param rv ReactiveValues list containing `resources`, `activities`, and `solution`
#' @param resources_proxy DT proxy for the `resources_table` in the main UI
#' @param activities_proxy DT proxy for the `activities_table` in the main UI
#' @importFrom shiny moduleServer observeEvent downloadHandler req
#' @importFrom DT replaceData
#' @importFrom utils read.csv write.csv
#' @importFrom ram define_resources define_activities create_ram_model solve_ram
#' @noRd
mod_solve_sidebar_server <- function(id, rv, resources_proxy, activities_proxy) {
  moduleServer(id, function(input, output, session) {
    
    # --- Download handlers for current templates ---
    output$download_resource_template <- downloadHandler(
      filename = "resource_template.csv",
      content = function(file) {
        write.csv(
          data.frame(
            resource     = rv$resources$resource,
            availability = rv$resources$availability,
            direction    = rv$resources$direction,
            stringsAsFactors = FALSE
          ),
          file,
          row.names = FALSE
        )
      }
    )
    output$download_activity_template <- downloadHandler(
      filename = "activity_template.csv",
      content = function(file) {
        write.csv(rv$activities, file, row.names = FALSE)
      }
    )
    
    # --- Upload & replace resources table ---
    observeEvent(input$resource_file, {
      req(input$resource_file)
      df <- read.csv(input$resource_file$datapath, stringsAsFactors = FALSE)
      parsed <- tryCatch(
        validate_resource_upload(df),
        error = function(err) {
          shiny::showNotification(
            paste("Resource file error:", err$message),
            type = "error"
          )
          NULL
        }
      )
      if (is.null(parsed)) {
        return()
      }
      rv$resources <- parsed
      rv$solution  <- NULL
      rv$solver_status <- list(
        state = "dirty",
        code = NA_integer_,
        message = "Resources replaced from upload. Re-run the solver."
      )
      DT::replaceData(resources_proxy, rv$resources, resetPaging = FALSE)
    })
    
    # --- Upload & replace activities table ---
    observeEvent(input$activity_file, {
      req(input$activity_file)
      df <- read.csv(input$activity_file$datapath, stringsAsFactors = FALSE)
      parsed <- tryCatch(
        validate_activity_upload(df),
        error = function(err) {
          shiny::showNotification(
            paste("Activity file error:", err$message),
            type = "error"
          )
          NULL
        }
      )
      if (is.null(parsed)) {
        return()
      }
      rv$activities <- parsed
      rv$solution    <- NULL
      rv$solver_status <- list(
        state = "dirty",
        code = NA_integer_,
        message = "Activities replaced from upload. Re-run the solver."
      )
      DT::replaceData(activities_proxy, rv$activities, resetPaging = FALSE)
    })
    
    # --- Solve the model when the button is clicked ---
    observeEvent(input$solve, {
      if (!nrow(rv$resources)) {
        shiny::showNotification("Please provide at least one resource before solving.", type = "error")
        return()
      }
      if (!nrow(rv$activities)) {
        shiny::showNotification("Please provide at least one activity before solving.", type = "error")
        return()
      }
      rv$solver_status <- list(
        state = "running",
        code = NA_integer_,
        message = "Solving…"
      )
      tryCatch(
        {
          res_def <- define_resources(
            resources    = rv$resources$resource,
            availability = as.numeric(rv$resources$availability),
            direction    = rv$resources$direction
          )
          acts     <- rv$activities
          techCols <- setdiff(names(acts), c("activity", "objective"))
          techMat  <- t(as.matrix(acts[, techCols, drop = FALSE]))
          colnames(techMat) <- acts$activity
          rownames(techMat) <- techCols
          act_def <- define_activities(
            activities                   = acts$activity,
            activity_requirements_matrix = techMat,
            objective                    = as.numeric(acts$objective)
          )
          model   <- create_ram_model(res_def, act_def)
          result  <- solve_ram(model, direction = input$direction)
          if (isTRUE(result$status == 0)) {
            rv$solution <- result
            rv$solver_status <- list(
              state = "success",
              code = result$status,
              message = sprintf("Optimal objective: %.2f", result$objective_value)
            )
          } else {
            rv$solution <- NULL
            rv$solver_status <- list(
              state = "error",
              code = result$status,
              message = paste("Solver did not converge. Status code:", result$status)
            )
            shiny::showNotification(
              paste("Solver did not converge. Status code:", result$status),
              type = "error"
            )
          }
        },
        error = function(err) {
          rv$solution <- NULL
          rv$solver_status <- list(
            state = "error",
            code = NA_integer_,
            message = paste("Unable to solve model:", err$message)
          )
          shiny::showNotification(
            paste("Unable to solve model:", err$message),
            type = "error"
          )
        }
      )
    })
    
  })
}

validate_resource_upload <- function(df) {
  required_cols <- c("resource", "availability", "direction")
  if (!all(required_cols %in% names(df))) {
    stop("Resource CSV must contain columns: resource, availability, direction.")
  }
  df <- df[, required_cols]
  df$resource <- trimws(as.character(df$resource))
  if (anyNA(df$resource) || any(df$resource == "")) {
    stop("All resources must have a non-empty name.")
  }
  df$availability <- suppressWarnings(as.numeric(df$availability))
  if (anyNA(df$availability)) {
    stop("Availability must be numeric for every resource.")
  }
  df$direction <- trimws(df$direction)
  valid_dir <- c("<=", ">=")
  if (!all(df$direction %in% valid_dir)) {
    stop("Direction must be either '<=' or '>=' for every resource.")
  }
  df
}

validate_activity_upload <- function(df) {
  required_cols <- c("activity", "objective")
  if (!all(required_cols %in% names(df))) {
    stop("Activity CSV must contain columns: activity and objective.")
  }
  df$activity <- trimws(as.character(df$activity))
  if (anyNA(df$activity) || any(df$activity == "")) {
    stop("All activities must have a non-empty name.")
  }
  df$objective <- suppressWarnings(as.numeric(df$objective))
  if (anyNA(df$objective)) {
    stop("Objective values must be numeric for every activity.")
  }
  tech_cols <- setdiff(names(df), required_cols)
  if (!length(tech_cols)) {
    stop("Activities CSV must include at least one resource column.")
  }
  for (col in tech_cols) {
    df[[col]] <- suppressWarnings(as.numeric(df[[col]]))
    if (anyNA(df[[col]])) {
      stop(sprintf("All entries in resource column '%s' must be numeric.", col))
    }
  }
  df
}
