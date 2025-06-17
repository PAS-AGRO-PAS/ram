# R/app_server.R

#' The application server-side
#'
#' @param input, output, session Internal parameters for {shiny}.
#' @import shiny
#' @importFrom DT renderDT dataTableProxy datatable DTOutput replaceData coerceValue
#' @import plotly
#' @importFrom ram define_resources define_activities create_ram_model solve_ram sensitivity_ram
#' @importFrom ram plot_ram
#' @importFrom utils read.csv write.csv
#' @importFrom stats setNames
#' @importFrom dplyr bind_rows
#' @noRd
app_server <- function(input, output, session) {
  
  # -- Dynamic logo based on light/dark mode --
  output$app_logo <- renderUI({
    logo_file <- if (isTRUE(input$color_mode == "dark")) {
      "www/logo.png"
    } else {
      "www/logo.png"
    }
    tags$div(
      style = "display:flex; align-items:center;",
      tags$img(src = "www/logo.png", height = "60px", alt = "PAS‑AGRO‑PAS"),
      tags$span(" PAS‑AGRO‑PAS – ram app", class = "fs-4 ms-2")
    )
  })
  
  # --- Reactive values for solve & builder modes ---
  rv <- reactiveValues(
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
    builder_resources  = data.frame(Name = character(), Availability = numeric(), Direction = character(), stringsAsFactors = FALSE),
    builder_activities = data.frame(Name = character(), Objective = numeric(), stringsAsFactors = FALSE)
  )
  
  # -- DataTable proxies for solve mode --
  resources_proxy  <- DT::dataTableProxy("resources_table")
  activities_proxy <- DT::dataTableProxy("activities_table")
  
  # -- Solve sidebar logic (modularized) --
  mod_solve_sidebar_server(
    id = "solve",
    rv = rv
  )
  
  # -- Render and edit solve-mode tables --
  output$resources_table <- renderDT({
    DT::datatable(rv$resources, rownames = FALSE, editable = TRUE)
  })
  output$activities_table <- renderDT({
    DT::datatable(rv$activities, rownames = FALSE, editable = TRUE)
  })
  
  observeEvent(input$resources_table_cell_edit, {
    info <- input$resources_table_cell_edit
    col <- names(rv$resources)[info$col]
    if (col %in% c("availability", "direction")) {
      rv$resources[info$row, col] <- DT::coerceValue(info$value, rv$resources[info$row, col])
      DT::replaceData(resources_proxy, rv$resources, resetPaging = FALSE)
      rv$solution <- NULL
    }
  })
  observeEvent(input$activities_table_cell_edit, {
    info <- input$activities_table_cell_edit
    col <- names(rv$activities)[info$col]
    if (col != "activity") {
      rv$activities[info$row, col] <- DT::coerceValue(info$value, rv$activities[info$row, col])
      DT::replaceData(activities_proxy, rv$activities, resetPaging = FALSE)
      rv$solution <- NULL
    }
  })
  
  # -- Download handlers for solve-mode --
  output$download_resources_csv <- downloadHandler(
    filename = "resources_table.csv",
    content = function(f) write.csv(rv$resources, f, row.names = FALSE)
  )
  output$download_activities_csv <- downloadHandler(
    filename = "activities_table.csv",
    content = function(f) write.csv(rv$activities, f, row.names = FALSE)
  )
  
  # -- Solve-mode results rendering --
  output$solution_tbl <- renderDT({
    req(rv$solution)
    sol <- rv$solution$optimal_activities
    df <- data.frame(Activity = names(sol), Level = as.numeric(sol), stringsAsFactors = FALSE)
    reqs <- rv$activities[match(df$Activity, rv$activities$activity), c("land", "labor", "nitrogen")]
    df$Land <- df$Level * reqs$land
    df$Labor <- df$Level * reqs$labor
    df$N <- df$Level * reqs$nitrogen
    total <- data.frame(Activity = "Total",
                        Level = sum(df$Level),
                        Land = sum(df$Land),
                        Labor = sum(df$Labor),
                        N = sum(df$N),
                        stringsAsFactors = FALSE)
    out <- rbind(df, total)
    DT::datatable(out, rownames = FALSE, options = list(dom = "t")) %>%
      DT::formatRound(c("Level", "Land", "Labor", "N"), 1)
  })
  output$objective_val <- renderPrint({
    req(rv$solution)
    rv$solution$objective_value
  })
  output$activity_plot <- renderPlotly({
    req(rv$solution)
    plot_ram(rv$solution)
  })
  output$sensitivity_tbl <- renderDT({
    req(rv$solution)
    out <- tryCatch(sensitivity_ram(rv$solution), error = function(e) NULL)
    if (is.null(out)) {
      DT::datatable(data.frame(Message = "No sensitivity"), rownames = FALSE)
    } else {
      DT::datatable(out)
    }
  })
  
  # -- Builder mode logic (inline) --
  observeEvent(input$add_res, {
    new <- data.frame(Name = input$res_name, Availability = input$res_avail, Direction = input$res_dir, stringsAsFactors = FALSE)
    rv$builder_resources <- bind_rows(rv$builder_resources, new)
  })
  observeEvent(input$del_res, {
    sel <- input$res_tbl_rows_selected
    if (length(sel)) {
      rv$builder_resources <- rv$builder_resources[-sel, ]
    }
  })
  observeEvent(input$add_act, {
    coefs <- sapply(rv$builder_resources$Name, function(nm) input[[paste0("coef_", nm)]])
    new <- data.frame(Name = input$act_name, Objective = input$act_obj, t(coefs), stringsAsFactors = FALSE)
    names(new)[-(1:2)] <- rv$builder_resources$Name
    rv$builder_activities <- bind_rows(rv$builder_activities, new)
  })
  observeEvent(input$del_act, {
    sel <- input$act_tbl_rows_selected
    if (length(sel)) {
      rv$builder_activities <- rv$builder_activities[-sel, ]
    }
  })
  
  output$coef_inputs <- renderUI({
    req(nrow(rv$builder_resources) > 0)
    lapply(rv$builder_resources$Name, function(nm) {
      numericInput(paste0("coef_", nm), label = nm, value = 0)
    })
  })
  
  output$res_tbl <- renderDT({
    DT::datatable(rv$builder_resources, selection = "single", rownames = FALSE)
  })
  output$act_tbl <- renderDT({
    DT::datatable(rv$builder_activities, selection = "single", rownames = FALSE)
  })
  
  output$download_res_csv <- downloadHandler(
    filename = "builder_resources.csv",
    content = function(f) write.csv(rv$builder_resources, f, row.names = FALSE)
  )
  output$download_res_xlsx <- downloadHandler(
    filename = "builder_resources.xlsx",
    content = function(f) openxlsx::write.xlsx(rv$builder_resources, f)
  )
  output$download_act_csv <- downloadHandler(
    filename = "builder_activities.csv",
    content = function(f) write.csv(rv$builder_activities, f, row.names = FALSE)
  )
  output$download_act_xlsx <- downloadHandler(
    filename = "builder_activities.xlsx",
    content = function(f) openxlsx::write.xlsx(rv$builder_activities, f)
  )
}
