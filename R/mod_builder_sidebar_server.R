# R/mod_builder_sidebar_server.R

#' builder_sidebar Server Functions
#'
#' @param id Character; the module id
#' @param rv ReactiveValues; shared values (resources, activities, etc.)
#' @param builder_res_proxy DT proxy for the resources DT
#' @param builder_act_proxy DT proxy for the activities DT
#' @importFrom shiny moduleServer observeEvent renderUI downloadHandler req
#' @importFrom DT replaceData
#' @importFrom utils write.csv
#' @importFrom openxlsx write.xlsx
#' @importFrom dplyr bind_rows
#' @noRd
mod_builder_sidebar_server <- function(id, rv, builder_res_proxy, builder_act_proxy) {
  moduleServer(id, function(input, output, session) {
    # Add a new resource and immediately update the DT
    observeEvent(input$add_res, {
      res_name <- trimws(if (is.null(input$res_name)) "" else input$res_name)
      if (!nzchar(res_name)) {
        shiny::showNotification("Please enter a resource name before adding.", type = "error")
        return()
      }
      if (res_name %in% rv$builder_resources$Name) {
        shiny::showNotification("Resource names must be unique.", type = "error")
        return()
      }
      new <- data.frame(
        Name         = res_name,
        Availability = input$res_avail,
        Direction    = input$res_dir,
        stringsAsFactors = FALSE
      )
      rv$builder_resources <- bind_rows(rv$builder_resources, new)
      DT::replaceData(builder_res_proxy, rv$builder_resources, resetPaging = FALSE)
      rv$builder_activities <- ensure_builder_activity_columns(
        rv$builder_activities,
        rv$builder_resources$Name
      )
      DT::replaceData(builder_act_proxy, rv$builder_activities, resetPaging = FALSE)
    })
    
    # Add a new activity and update its DT
    observeEvent(input$add_act, {
      act_name <- trimws(if (is.null(input$act_name)) "" else input$act_name)
      if (!nzchar(act_name)) {
        shiny::showNotification("Please enter an activity name before adding.", type = "error")
        return()
      }
      if (act_name %in% rv$builder_activities$Name) {
        shiny::showNotification("Activity names must be unique.", type = "error")
        return()
      }
      if (!nrow(rv$builder_resources)) {
        shiny::showNotification("Add at least one resource before defining activities.", type = "error")
        return()
      }
      coefs <- vapply(
        rv$builder_resources$Name,
        function(nm) {
          val <- input[[paste0("coef_", nm)]]
          if (is.null(val)) 0 else val
        },
        numeric(1)
      )
      new <- data.frame(
        Name      = act_name,
        Objective = input$act_obj,
        t(coefs),
        stringsAsFactors = FALSE
      )
      names(new)[-(1:2)] <- rv$builder_resources$Name
      rv$builder_activities <- bind_rows(rv$builder_activities, new)
      DT::replaceData(builder_act_proxy, rv$builder_activities, resetPaging = FALSE)
    })
    
    # Dynamic coefficient inputs
    output$coef_inputs <- renderUI({
      req(nrow(rv$builder_resources) > 0)
      lapply(rv$builder_resources$Name, function(nm) {
        numericInput(session$ns(paste0("coef_", nm)), label = nm, value = 0)
      })
    })
    
    # Download handlers
    output$download_res_csv <- downloadHandler(
      filename = "builder_resources.csv",
      content  = function(file) write.csv(rv$builder_resources, file, row.names = FALSE)
    )
    output$download_res_xlsx <- downloadHandler(
      filename = "builder_resources.xlsx",
      content  = function(file) openxlsx::write.xlsx(rv$builder_resources, file)
    )
    output$download_act_csv <- downloadHandler(
      filename = "builder_activities.csv",
      content  = function(file) write.csv(rv$builder_activities, file, row.names = FALSE)
    )
    output$download_act_xlsx <- downloadHandler(
      filename = "builder_activities.xlsx",
      content  = function(file) openxlsx::write.xlsx(rv$builder_activities, file)
    )
    
    observeEvent(input$handoff_solver, {
      if (!nrow(rv$builder_resources) || !nrow(rv$builder_activities)) {
        shiny::showNotification("Add at least one resource and activity before opening the solver.", type = "error")
        return()
      }
      solver_resources <- builder_resources_to_solver(rv$builder_resources)
      solver_activities <- builder_activities_to_solver(
        rv$builder_activities,
        solver_resources$resource
      )
      payload <- list(
        resources = solver_resources,
        activities = solver_activities
      )
      json_txt <- jsonlite::toJSON(payload, dataframe = "rows", auto_unbox = TRUE, digits = NA)
      # Base64-encode so query parsing never splits on '=' or '&' inside the JSON
      payload_b64 <- jsonlite::base64_enc(json_txt)
      session$sendCustomMessage(
        "ram-open-solver",
        list(payload = payload_b64, encoding = "base64")
      )
      shiny::showNotification("Opening solver with your current builder tables...", type = "message")
    })
  })
}

ensure_builder_activity_columns <- function(df, resource_names) {
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  base_cols <- c("Name", "Objective")
  clean_resources <- unique(trimws(resource_names))
  clean_resources <- clean_resources[!is.na(clean_resources) & nzchar(clean_resources)]
  n_rows <- nrow(df)
  if (is.null(n_rows) || length(n_rows) == 0 || is.na(n_rows)) {
    n_rows <- 0
  }
  if (!all(base_cols %in% names(df))) {
    missing_base <- setdiff(base_cols, names(df))
    defaults <- list(
      Name = rep("", n_rows),
      Objective = rep(0, n_rows)
    )
    df[missing_base] <- defaults[missing_base]
  }
  if (!length(clean_resources)) {
    return(df[, intersect(names(df), base_cols), drop = FALSE])
  }
  missing <- setdiff(clean_resources, names(df))
  if (length(missing)) {
    # Fill all missing resource columns at once; works even with zero rows
    df[missing] <- replicate(length(missing), numeric(n_rows), simplify = FALSE)
  }
  ordered_cols <- c(base_cols, clean_resources)
  keep <- intersect(ordered_cols, names(df))
  df[, keep, drop = FALSE]
}

builder_resources_to_solver <- function(df) {
  out <- data.frame(
    resource     = df$Name,
    availability = df$Availability,
    direction    = df$Direction,
    stringsAsFactors = FALSE
  )
  validate_resource_upload(out)
}

builder_activities_to_solver <- function(df, resource_names) {
  out <- df
  names(out)[match("Name", names(out))] <- "activity"
  names(out)[match("Objective", names(out))] <- "objective"
  missing <- setdiff(resource_names, names(out))
  if (length(missing)) {
    stop("Missing coefficients for: ", paste(missing, collapse = ", "))
  }
  ordered_cols <- c("activity", resource_names, "objective")
  out <- out[, ordered_cols, drop = FALSE]
  validate_activity_upload(out)
}
