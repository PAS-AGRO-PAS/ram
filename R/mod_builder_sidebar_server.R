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
      req(input$res_name)
      new <- data.frame(
        Name         = input$res_name,
        Availability = input$res_avail,
        Direction    = input$res_dir,
        stringsAsFactors = FALSE
      )
      rv$builder_resources <- bind_rows(rv$builder_resources, new)
      DT::replaceData(builder_res_proxy, rv$builder_resources, resetPaging = FALSE)
    })
    
    # Remove selected resource and update the DT
    observeEvent(input$del_res, {
      sel <- input$res_tbl_rows_selected
      if (length(sel)) {
        rv$builder_resources <- rv$builder_resources[-sel, , drop = FALSE]
        DT::replaceData(builder_res_proxy, rv$builder_resources, resetPaging = FALSE)
      }
    })
    
    # Add a new activity and update its DT
    observeEvent(input$add_act, {
      req(input$act_name)
      coefs <- sapply(rv$builder_resources$Name, function(nm) input[[paste0("coef_", nm)]])
      new <- data.frame(
        Name      = input$act_name,
        Objective = input$act_obj,
        t(coefs),
        stringsAsFactors = FALSE
      )
      names(new)[-(1:2)] <- rv$builder_resources$Name
      rv$builder_activities <- bind_rows(rv$builder_activities, new)
      DT::replaceData(builder_act_proxy, rv$builder_activities, resetPaging = FALSE)
    })
    
    # Remove selected activity and update its DT
    observeEvent(input$del_act, {
      sel <- input$act_tbl_rows_selected
      if (length(sel)) {
        rv$builder_activities <- rv$builder_activities[-sel, , drop = FALSE]
        DT::replaceData(builder_act_proxy, rv$builder_activities, resetPaging = FALSE)
      }
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
  })
}
