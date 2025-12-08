# R/app_server.R

#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}. DO NOT REMOVE.
#' @importFrom shiny renderUI reactiveValues observeEvent renderPrint req
#' @importFrom shiny moduleServer
#' @importFrom DT renderDT dataTableProxy replaceData
#' @importFrom golem get_golem_options
#' @importFrom ram define_resources define_activities create_ram_model solve_ram sensitivity_ram
#' @importFrom ram plot_ram
#' @importFrom utils read.csv write.csv
#' @importFrom openxlsx write.xlsx
#' @importFrom dplyr bind_rows
#' @importFrom plotly renderPlotly
#' @export
app_server <- function(input, output, session) {
  
  # -- Dynamic logo based on dark/light toggle --
  output$app_logo <- renderUI({
    tags$div(
      style = "display:flex; align-items:center;",
      tags$img(src = "www/logo.png", height = "60px", alt = "PAS-AGRO-PAS"),
      tags$span(" PAS-AGRO-PAS – ram app", class = "fs-4 ms-2")
    )
  })
  
  # --- Shared reactiveValues for both modes ---
  rv <- reactiveValues(
    ## Solve-mode
    resources = data.frame(
      resource     = c("land", "labor", "nitrogen",
                       "oat_min", "oat_max", "barley_min", "barley_max",
                       "lupin_min", "lupin_max", "fava_min", "fava_max",
                       "pasture_min", "pasture_max"),
      availability = c(15, 350, 500, 2, 8, 1, 6, 2, 5, 3, 7, 0, 10),
      direction    = c(
                       rep("<=", 3),
                       ">=", "<=",  # oat
                       ">=", "<=",  # barley
                       ">=", "<=",  # lupin
                       ">=", "<=",  # fava
                       ">=", "<="   # pasture
                       ),
      stringsAsFactors = FALSE
    ),
    activities = data.frame(
      activity    = c("oat", "barley", "lupin", "fava", "pasture"),
      land        = rep(1, 5),
      labor       = c(20, 25, 15, 30, 10),
      nitrogen    = c(80, 100, 0, 30, 0),
      oat_min     = c(1, 0, 0, 0, 0), oat_max = c(1, 0, 0, 0, 0),
      barley_min  = c(0, 1, 0, 0, 0), barley_max = c(0, 1, 0, 0, 0),
      lupin_min   = c(0, 0, 1, 0, 0), lupin_max = c(0, 0, 1, 0, 0),
      fava_min    = c(0, 0, 0, 1, 0), fava_max = c(0, 0, 0, 1, 0),
      pasture_min = c(0, 0, 0, 0, 1), pasture_max = c(0, 0, 0, 0, 1),
      objective   = c(400, 450, 350, 500, 360),
      stringsAsFactors = FALSE
    ),
    solution = NULL,
    builder_resources  = data.frame(Name = character(), 
                                    Availability = numeric(),
                                    Direction = character(),
                                    stringsAsFactors = FALSE),
    builder_activities = data.frame(Name = character(), 
                                    Objective = numeric(), 
                                    stringsAsFactors = FALSE),
    solver_status = list(state = "idle",
                         code = NA_integer_,
                         message = "Awaiting first solve")
    )
  
  # --- DT proxies for solve-mode ---
  resources_proxy  <- DT::dataTableProxy("resources_table")
  activities_proxy <- DT::dataTableProxy("activities_table")
  observeEvent(session$clientData$url_search, {
    url <- session$clientData$url_search %||% ""
    payload <- parse_builder_payload(url)
    if (!is.null(payload)) {
      rv$resources <- payload$resources
      rv$activities <- payload$activities
      rv$solution <- NULL
      rv$solver_status <- list(
        state = "dirty",
        code = NA_integer_,
        message = "Loaded tables from builder handoff. Run the solver to compute results."
      )
      DT::replaceData(resources_proxy, rv$resources, resetPaging = FALSE)
      DT::replaceData(activities_proxy, rv$activities, resetPaging = FALSE)
      shiny::showNotification("Builder tables loaded. Switch to the Optimization tab and click Solve.", type = "message")
    } else if (grepl("builder_payload=", url, fixed = TRUE)) {
      shiny::showNotification("Builder payload was present but could not be parsed.", type = "error")
    }
  }, once = TRUE)
  
  # --- Solve sidebar module (uploads, downloads, solve) ---
  mod_solve_sidebar_server(
    id               = "solve",
    rv               = rv,
    resources_proxy  = resources_proxy,
    activities_proxy = activities_proxy
  )
  
  # --- Render solve-mode editable tables ---
  output$resources_table <- DT::renderDT({
    DT::datatable(rv$resources, rownames = FALSE, editable = TRUE)
  })
  output$activities_table <- DT::renderDT({
    DT::datatable(rv$activities, rownames = FALSE, editable = TRUE)
  })
  # Keep tables alive when tabs are hidden and keep them in sync with rv
  outputOptions(output, "resources_table", suspendWhenHidden = FALSE)
  outputOptions(output, "activities_table", suspendWhenHidden = FALSE)
  observeEvent(input$resources_table_cell_edit, {
    info <- input$resources_table_cell_edit
    prev <- rv$resources
    edited <- DT::editData(rv$resources, info, proxy = resources_proxy, resetPaging = FALSE)
    parsed <- tryCatch(
      validate_resource_upload(edited),
      error = function(err) {
        shiny::showNotification(paste("Invalid resource edit:", err$message), type = "error")
        DT::replaceData(resources_proxy, prev, resetPaging = FALSE)
        rv$solver_status <- list(
          state = "error",
          code = NA_integer_,
          message = err$message
        )
        return(NULL)
      }
    )
    if (is.null(parsed)) {
      return()
    }
    rv$resources <- parsed
    DT::replaceData(resources_proxy, rv$resources, resetPaging = FALSE)
    rv$solution <- NULL
    rv$solver_status <- list(
      state = "dirty",
      code = NA_integer_,
      message = "Resource table edited. Re-run the solver."
    )
  })
  observeEvent(input$activities_table_cell_edit, {
    info <- input$activities_table_cell_edit
    prev <- rv$activities
    edited <- DT::editData(rv$activities, info, proxy = activities_proxy, resetPaging = FALSE)
    parsed <- tryCatch(
      validate_activity_upload(edited),
      error = function(err) {
        shiny::showNotification(paste("Invalid activity edit:", err$message), type = "error")
        DT::replaceData(activities_proxy, prev, resetPaging = FALSE)
        rv$solver_status <- list(
          state = "error",
          code = NA_integer_,
          message = err$message
        )
        return(NULL)
      }
    )
    if (is.null(parsed)) {
      return()
    }
    rv$activities <- parsed
    DT::replaceData(activities_proxy, rv$activities, resetPaging = FALSE)
    rv$solution <- NULL
    rv$solver_status <- list(
      state = "dirty",
      code = NA_integer_,
      message = "Activity table edited. Re-run the solver."
    )
  })
  
  # --- Solve-mode results ---
  output$solution_tbl <- DT::renderDT({
    req(rv$solution)
    sol_vec <- rv$solution$optimal_activities
    df <- data.frame(
      Activity = names(sol_vec),
      Level    = as.numeric(sol_vec),
      stringsAsFactors = FALSE
    )
    tech_cols <- setdiff(names(rv$activities), c("activity", "objective"))
    if (length(tech_cols)) {
      reqs <- rv$activities[match(df$Activity, rv$activities$activity), tech_cols, drop = FALSE]
      reqs[] <- lapply(reqs, as.numeric)
      usage <- sweep(reqs, 1, df$Level, `*`)
      df <- cbind(df, usage)
      total_usage <- colSums(usage)
      total <- data.frame(
        Activity = "Total",
        Level = sum(df$Level),
        t(total_usage),
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
      df <- rbind(df, total, stringsAsFactors = FALSE)
      numeric_cols <- setdiff(names(df), "Activity")
      DT::datatable(
        df,
        rownames = FALSE,
        options = list(dom = "t", scrollX = TRUE)
      ) %>%
        DT::formatRound(numeric_cols, 1)
    } else {
      total <- data.frame(
        Activity = "Total",
        Level = sum(df$Level),
        stringsAsFactors = FALSE
      )
      DT::datatable(
        rbind(df, total),
        rownames = FALSE,
        options = list(dom = "t", scrollX = TRUE)
      ) %>%
        DT::formatRound("Level", 1)
    }
  })
  output$objective_val   <- renderPrint({ req(rv$solution); rv$solution$objective_value })
  output$activity_plot   <- plotly::renderPlotly({ req(rv$solution); plot_ram(rv$solution) })
  output$sensitivity_tbl <- DT::renderDT({
    req(rv$solution)
    out <- tryCatch(sensitivity_ram(rv$solution), error = function(e) NULL)
    if (is.null(out)) {
      DT::datatable(data.frame(Message = "No sensitivity"), rownames = FALSE)
    } else {
      DT::datatable(out)
    }
  })
  output$solver_status <- renderUI({
    status <- rv$solver_status
    state  <- status$state %||% "idle"
    label  <- switch(
      state,
      success = "Optimal solution",
      error   = "Solver issue",
      running = "Solving…",
      dirty   = "Needs solve",
      "Idle"
    )
    badge_class <- switch(
      state,
      success = "bg-success",
      error   = "bg-danger",
      running = "bg-warning text-dark",
      dirty   = "bg-secondary",
      "bg-secondary"
    )
    tags$div(
      class = "mb-2",
      tags$span(class = paste("badge", badge_class), label),
      tags$span(class = "ms-2", status$message %||% "Run the solver to see output.")
    )
  })
  output$solver_diagnostics <- renderPrint({
    status <- rv$solver_status
    state  <- status$state %||% "idle"
    message <- status$message %||% "Awaiting first solve."
    cat(message, "\n")
    if (!is.null(status$code) && !is.na(status$code)) {
      cat("lpSolve status code:", status$code, "\n")
    }
    if (state == "error") {
      cat("Hints: Verify resource bounds, ensure at least one feasible activity combination, and review constraint directions.\n")
    }
  })
  output$download_lp_report <- downloadHandler(
    filename = function() {
      paste0("ram-solver-report-", Sys.Date(), ".rds")
    },
    content = function(file) {
      req(rv$solution)
      saveRDS(rv$solution$lp_result, file = file)
    }
  )
  
  # --- DT proxies for builder-mode ---
  builder_res_proxy <- DT::dataTableProxy("res_tbl")
  builder_act_proxy <- DT::dataTableProxy("act_tbl")
  
  # --- Builder sidebar module (add/remove, dynamic UI, exports) ---
  mod_builder_sidebar_server(
    id                = "builder",
    rv                = rv,
    builder_res_proxy = builder_res_proxy,
    builder_act_proxy = builder_act_proxy
  )
  
  # --- Render builder-mode tables ---
  output$res_tbl <- DT::renderDT({
    DT::datatable(rv$builder_resources, selection = "single", rownames = FALSE)
  })
  output$act_tbl <- DT::renderDT({
    DT::datatable(rv$builder_activities, selection = "single", rownames = FALSE)
  })
  
  # --- Wire up the namespaced remove buttons ---
  observeEvent(input[["builder-del_res"]], {
    sel <- input$res_tbl_rows_selected
    if (length(sel)) {
      removed <- rv$builder_resources$Name[sel]
      rv$builder_resources <- rv$builder_resources[-sel, , drop = FALSE]
      DT::replaceData(builder_res_proxy, rv$builder_resources, resetPaging = FALSE)
      if (length(removed)) {
        keep_cols <- setdiff(names(rv$builder_activities), removed)
        rv$builder_activities <- rv$builder_activities[, keep_cols, drop = FALSE]
        rv$builder_activities <- ensure_builder_activity_columns(
          rv$builder_activities,
          rv$builder_resources$Name
        )
        DT::replaceData(builder_act_proxy, rv$builder_activities, resetPaging = FALSE)
      }
    }
  })
  observeEvent(input[["builder-del_act"]], {
    sel <- input$act_tbl_rows_selected
    if (length(sel)) {
      rv$builder_activities <- rv$builder_activities[-sel, , drop = FALSE]
      DT::replaceData(builder_act_proxy, rv$builder_activities, resetPaging = FALSE)
    }
  })
}

`%||%` <- function(x, y) {
  if (is.null(x)) {
    y
  } else {
    x
  }
}

parse_builder_payload <- function(url_search) {
  if (is.null(url_search) || !nzchar(url_search)) {
    return(NULL)
  }
  query <- shiny::parseQueryString(sub("^\\?", "", url_search))
  payload_raw <- query$builder_payload
  if (is.null(payload_raw) || !nzchar(payload_raw)) {
    return(NULL)
  }
  # Payload arrives URL-encoded; we also support base64-wrapped JSON to
  # avoid '=' characters breaking query parsing.
  decoded <- utils::URLdecode(payload_raw)
  json_txt <- tryCatch(
    {
      raw_txt <- jsonlite::base64_dec(decoded)
      rawToChar(raw_txt)
    },
    error = function(e) decoded
  )
  payload <- tryCatch(
    jsonlite::fromJSON(json_txt, simplifyDataFrame = TRUE),
    error = function(e) NULL
  )
  if (is.null(payload$resources) || is.null(payload$activities)) {
    return(NULL)
  }
  resources <- as.data.frame(payload$resources, stringsAsFactors = FALSE)
  activities <- as.data.frame(payload$activities, stringsAsFactors = FALSE)
  resources <- tryCatch(validate_resource_upload(resources), error = function(e) NULL)
  activities <- tryCatch(validate_activity_upload(activities), error = function(e) NULL)
  if (is.null(resources) || is.null(activities)) {
    return(NULL)
  }
  list(resources = resources, activities = activities)
}
