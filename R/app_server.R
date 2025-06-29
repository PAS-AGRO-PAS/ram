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
                                    stringsAsFactors = FALSE)
    )
  
  # --- DT proxies for solve-mode ---
  resources_proxy  <- DT::dataTableProxy("resources_table")
  activities_proxy <- DT::dataTableProxy("activities_table")
  
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
  
  # --- Solve-mode results ---
  output$solution_tbl <- DT::renderDT({
    req(rv$solution)
    sol_vec <- rv$solution$optimal_activities
    df <- data.frame(
      Activity = names(sol_vec),
      Level    = as.numeric(sol_vec),
      stringsAsFactors = FALSE
    )
    reqs <- rv$activities[match(df$Activity, rv$activities$activity),
                          c("land","labor","nitrogen")]
    df$Land  <- df$Level * reqs$land
    df$Labor <- df$Level * reqs$labor
    df$N     <- df$Level * reqs$nitrogen
    total <- data.frame(
      Activity = "Total",
      Level    = sum(df$Level),
      Land     = sum(df$Land),
      Labor    = sum(df$Labor),
      N        = sum(df$N),
      stringsAsFactors = FALSE
    )
    DT::datatable(
      rbind(df, total),
      rownames = FALSE,
      options = list(dom = "t")
    ) %>%
      DT::formatRound(c("Level","Land","Labor","N"), 1)
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
      rv$builder_resources <- rv$builder_resources[-sel, , drop = FALSE]
      DT::replaceData(builder_res_proxy, rv$builder_resources, resetPaging = FALSE)
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
