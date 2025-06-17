# R/mod_solve_sidebar_server.R

#' Solve Sidebar Server Module
#'
#' @description Server logic for solve-mode sidebar (upload, download, solve).
#' @param id Module namespace ID.
#' @param rv ReactiveValues list containing `resources`, `activities`, and `solution`.
#' @noRd
#' @importFrom shiny moduleServer downloadHandler observeEvent req
#' @importFrom DT dataTableProxy replaceData
#' @importFrom ram define_resources define_activities create_ram_model solve_ram

mod_solve_sidebar_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Internal proxies for editable tables
    resources_proxy  <- DT::dataTableProxy("resources_table", session = session)
    activities_proxy <- DT::dataTableProxy("activities_table", session = session)
    
    # Download current resource template
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
    
    # Download current activity template
    output$download_activity_template <- downloadHandler(
      filename = "activity_template.csv",
      content = function(file) {
        write.csv(rv$activities, file, row.names = FALSE)
      }
    )
    
    # Upload resources CSV
    observeEvent(input$resource_file, {
      req(input$resource_file)
      df <- utils::read.csv(input$resource_file$datapath, stringsAsFactors = FALSE)
      req(all(c("resource", "availability", "direction") %in% names(df)))
      
      rv$resources <- df
      rv$solution  <- NULL
      DT::replaceData(resources_proxy, rv$resources, resetPaging = FALSE)
    })
    
    # Upload activities CSV
    observeEvent(input$activity_file, {
      req(input$activity_file)
      df <- utils::read.csv(input$activity_file$datapath, stringsAsFactors = FALSE)
      req(all(c("activity", "objective") %in% names(df)))
      
      rv$activities <- df
      rv$solution   <- NULL
      DT::replaceData(activities_proxy, rv$activities, resetPaging = FALSE)
    })
    
    # Solve the model on Solve button click
    observeEvent(input$solve, {
      # Build resource definitions
      res_def <- ram::define_resources(
        resources    = rv$resources$resource,
        availability = as.numeric(rv$resources$availability),
        direction    = rv$resources$direction
      )
      
      # Build activity definitions
      acts <- rv$activities
      techCols <- setdiff(names(acts), c("activity", "objective"))
      techMat <- t(as.matrix(acts[, techCols]))
      colnames(techMat) <- acts$activity
      rownames(techMat) <- techCols
      
      act_def <- ram::define_activities(
        activities                   = acts$activity,
        activity_requirements_matrix = techMat,
        objective                    = as.numeric(acts$objective)
      )
      
      # Solve
      model <- ram::create_ram_model(res_def, act_def)
      rv$solution <- ram::solve_ram(model, direction = input$direction)
    })
  })
}
