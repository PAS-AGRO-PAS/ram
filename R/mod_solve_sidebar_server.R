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
      req(all(c("resource", "availability", "direction") %in% names(df)))
      rv$resources <- df
      rv$solution  <- NULL
      DT::replaceData(resources_proxy, rv$resources, resetPaging = FALSE)
    })
    
    # --- Upload & replace activities table ---
    observeEvent(input$activity_file, {
      req(input$activity_file)
      df <- read.csv(input$activity_file$datapath, stringsAsFactors = FALSE)
      req(all(c("activity", "objective") %in% names(df)))
      rv$activities <- df
      rv$solution    <- NULL
      DT::replaceData(activities_proxy, rv$activities, resetPaging = FALSE)
    })
    
    # --- Solve the model when the button is clicked ---
    observeEvent(input$solve, {
      # build resource definition
      res_def <- define_resources(
        resources    = rv$resources$resource,
        availability = as.numeric(rv$resources$availability),
        direction    = rv$resources$direction
      )
      # build activity definition
      acts    <- rv$activities
      techCols <- setdiff(names(acts), c("activity", "objective"))
      techMat  <- t(as.matrix(acts[, techCols, drop = FALSE]))
      colnames(techMat) <- acts$activity
      rownames(techMat) <- techCols
      act_def <- define_activities(
        activities                   = acts$activity,
        activity_requirements_matrix = techMat,
        objective                    = as.numeric(acts$objective)
      )
      # create & solve
      model      <- create_ram_model(res_def, act_def)
      rv$solution <- solve_ram(model, direction = input$direction)
    })
    
  })
}
