#' Solve Sidebar Server Module
#'
#' @noRd
#' @importFrom shiny moduleServer downloadHandler observeEvent req
#' @importFrom DT replaceData
mod_solve_sidebar_server <- function(id, rv, resources_proxy, activities_proxy) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$download_resource_template <- downloadHandler(
      filename = "resource_template.csv",
      content = function(file) {
        write.csv(
          data.frame(
            resource = rv$resources$resource,
            availability = rv$resources$availability,
            direction = rv$resources$direction,
            stringsAsFactors = FALSE
          ),
          file, row.names = FALSE
        )
      }
    )
    
    output$download_activity_template <- downloadHandler(
      filename = "activity_template.csv",
      content = function(file) {
        acts <- rv$activities
        write.csv(acts, file, row.names = FALSE)
      }
    )
    
    observeEvent(input$resource_file, {
      df <- read.csv(input$resource_file$datapath, stringsAsFactors = FALSE)
      req(all(c("resource","availability","direction") %in% names(df)))
      rv$resources <- df
      rv$solution <- NULL
      replaceData(resources_proxy, rv$resources, resetPaging = FALSE)
    })
    
    observeEvent(input$activity_file, {
      df <- read.csv(input$activity_file$datapath, stringsAsFactors = FALSE)
      req(all(c("activity","objective") %in% names(df)))
      rv$activities <- df
      rv$solution <- NULL
      replaceData(activities_proxy, rv$activities, resetPaging = FALSE)
    })
    
    observeEvent(input$solve, {
      # Re-run solve logic from app_server
      res_def <- define_resources(
        resources = rv$resources$resource,
        availability = as.numeric(rv$resources$availability),
        direction = rv$resources$direction
      )
      acts <- rv$activities
      techCols <- setdiff(names(acts), c("activity","objective"))
      techMat <- t(as.matrix(acts[, techCols]))
      colnames(techMat) <- acts$activity
      rownames(techMat) <- techCols
      act_def <- define_activities(
        activities = acts$activity,
        activity_requirements_matrix = techMat,
        objective = as.numeric(acts$objective)
      )
      model <- create_ram_model(res_def, act_def)
      rv$solution <- solve_ram(model, direction = input$direction)
    })
  })
}
