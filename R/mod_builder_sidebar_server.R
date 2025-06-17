
#' builder_sidebar Server Functions
#'
#' @noRd 
mod_builder_sidebar_server <- function(id, rv, builder_res_proxy, builder_act_proxy) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    observeEvent(input$add_res, {
      new <- data.frame(Name = input$res_name, Availability = input$res_avail,
                        Direction = input$res_dir, stringsAsFactors = FALSE)
      rv$builder_resources <- bind_rows(rv$builder_resources, new)
    })
    observeEvent(input$del_res, {
      sel <- input$res_tbl_rows_selected
      if (length(sel)) rv$builder_resources <- rv$builder_resources[-sel, ]
    })
    
    observeEvent(input$add_act, {
      coefs <- sapply(rv$builder_resources$Name, function(nm) input[[paste0("coef_", nm)]])
      new <- data.frame(Name = input$act_name, Objective = input$act_obj, t(coefs), stringsAsFactors = FALSE)
      names(new)[-(1:2)] <- rv$builder_resources$Name
      rv$builder_activities <- bind_rows(rv$builder_activities, new)
    })
    observeEvent(input$del_act, {
      sel <- input$act_tbl_rows_selected
      if (length(sel)) rv$builder_activities <- rv$builder_activities[-sel, ]
    })
    
    output$coef_inputs <- renderUI({
      req(nrow(rv$builder_resources) > 0)
      lapply(rv$builder_resources$Name, function(nm) {
        numericInput(ns(paste0("coef_", nm)), label = nm, value = 0)
      })
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
  })
}
