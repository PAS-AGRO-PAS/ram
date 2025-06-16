# R/app_server.R

#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom DT renderDT dataTableProxy datatable
#' @importFrom DT DTOutput replaceData coerceValue
#' @import plotly
#' @importFrom ram define_resources define_activities create_ram_model solve_ram sensitivity_ram
#' @importFrom ram plot_ram
#' @importFrom utils read.csv write.csv
#' @importFrom stats setNames
#' @importFrom dplyr bind_rows
#' @noRd
app_server <- function(input, output, session) {

  # --- Downloadable CSV templates for the five‐crop + pasture example ---
  output$download_resource_template <- downloadHandler(
    filename = "resource_template.csv",
    content = function(file) {
      write.csv(
        data.frame(
          resource     = c(
            # totals
            "land", "labor", "nitrogen",
            # per‐crop bounds (min, max)
            "oat_min", "oat_max",
            "barley_min", "barley_max",
            "lupin_min", "lupin_max",
            "fava_min", "fava_max",
            # pasture bounds
            "pasture_min", "pasture_max"
          ),
          availability = c(
            # totals
            15,    350,    500,
            # oat
            2,      8,
            # barley
            1,      6,
            # lupin
            2,      5,
            # fava
            3,      7,
            # pasture
            0,     10
          ),
          direction    = c(
            # totals all <=
            "<=", "<=", "<=",
            # bounds: min >=, max <=
            ">=", "<=",
            ">=", "<=",
            ">=", "<=",
            ">=", "<=",
            ">=", "<="
          ),
          stringsAsFactors = FALSE
        ),
        file, row.names = FALSE
      )
    }
  )

  output$download_activity_template <- downloadHandler(
    filename = "activity_template.csv",
    content = function(file) {
      write.csv(
        data.frame(
          activity     = c("oat","barley","lupin","fava","pasture"),
          # technical matrix columns must match the resources in the same order:
          land         = c(1, 1, 1, 1, 1),
          labor        = c(20,25,15,30,10),
          nitrogen     = c(80,100,0,30,0),
          oat_min      = c(1,0,0,0,0),
          oat_max      = c(1,0,0,0,0),
          barley_min   = c(0,1,0,0,0),
          barley_max   = c(0,1,0,0,0),
          lupin_min    = c(0,0,1,0,0),
          lupin_max    = c(0,0,1,0,0),
          fava_min     = c(0,0,0,1,0),
          fava_max     = c(0,0,0,1,0),
          pasture_min  = c(0,0,0,0,1),
          pasture_max  = c(0,0,0,0,1),
          objective    = c(400,450,350,500,360),
          stringsAsFactors = FALSE
        ),
        file, row.names = FALSE
      )
    }
  )

  # --- Reactive values for the two DTs and solution ---
  rv <- reactiveValues(
    resources = data.frame(
      resource     = c("land","labor","nitrogen",
                       "oat_min","oat_max",
                       "barley_min","barley_max",
                       "lupin_min","lupin_max",
                       "fava_min","fava_max",
                       "pasture_min","pasture_max"),
      availability = c(15,350,500, 2,8, 1,6, 2,5, 3,7, 0,10),
      direction    = c(rep("<=",3),
                       ">=", "<=",  # oat
                       ">=", "<=",  # barley
                       ">=", "<=",  # lupin
                       ">=", "<=",  # fava
                       ">=", "<="), # pasture
      stringsAsFactors = FALSE
    ),
    activities = data.frame(
      activity    = c("oat","barley","lupin","fava","pasture"),
      land        = c(1,1,1,1,1),
      labor       = c(20,25,15,30,10),
      nitrogen    = c(80,100,0,30,0),
      oat_min     = c(1,0,0,0,0),
      oat_max     = c(1,0,0,0,0),
      barley_min  = c(0,1,0,0,0),
      barley_max  = c(0,1,0,0,0),
      lupin_min   = c(0,0,1,0,0),
      lupin_max   = c(0,0,1,0,0),
      fava_min    = c(0,0,0,1,0),
      fava_max    = c(0,0,0,1,0),
      pasture_min = c(0,0,0,0,1),
      pasture_max = c(0,0,0,0,1),
      objective   = c(400,450,350,500,360),
      stringsAsFactors = FALSE
    ),
    solution = NULL
  )

  # --- DT proxies for client-side edits ---
  resources_proxy  <- dataTableProxy("resources_table")
  activities_proxy <- dataTableProxy("activities_table")

  # --- Upload CSVs & clear any previous solution ---
  observeEvent(input$resource_file, {
    req(input$resource_file)
    df <- read.csv(input$resource_file$datapath, stringsAsFactors = FALSE)
    req(all(c("resource","availability","direction") %in% names(df)))
    rv$resources <- df
    rv$solution  <- NULL
  })
  observeEvent(input$activity_file, {
    req(input$activity_file)
    df <- read.csv(input$activity_file$datapath, stringsAsFactors = FALSE)
    req(all(c("activity","objective") %in% names(df)))
    rv$activities <- df
    rv$solution    <- NULL
  })

  # --- Add / remove rows ---
  observeEvent(input$add_resource, {
    rv$resources <- bind_rows(
      rv$resources,
      data.frame(resource=NA, availability=NA, direction="<=", stringsAsFactors=FALSE)
    )
    rv$solution <- NULL
  })
  observeEvent(input$del_resource, {
    sel <- input$resources_table_rows_selected
    if (length(sel)) {
      rv$resources <- rv$resources[-sel, ]
      rv$solution  <- NULL
    }
  })
  observeEvent(input$add_activity, {
    empty <- as.list(rep(NA, ncol(rv$activities)))
    names(empty) <- names(rv$activities)
    empty$activity  <- NA
    empty$objective <- NA
    rv$activities  <- bind_rows(rv$activities, empty)
    rv$solution     <- NULL
  })
  observeEvent(input$del_activity, {
    sel <- input$activities_table_rows_selected
    if (length(sel)) {
      rv$activities <- rv$activities[-sel, ]
      rv$solution    <- NULL
    }
  })

  # --- Render the editable DTs ---
  output$resources_table <- DT::renderDT({
    DT::datatable(
      rv$resources,
      rownames = FALSE,
      selection = "single",
      editable = TRUE    # <- everything editable
    )
  })

  output$activities_table <- DT::renderDT({
    DT::datatable(
      rv$activities,
      rownames = FALSE,
      selection = "single",
      editable = TRUE    # <- everything editable
    )
  })

  # --- Handle cell edits & clear solution ---
  observeEvent(input$resources_table_cell_edit, {
    info     <- input$resources_table_cell_edit
    col_name <- names(rv$resources)[ info$col ]
    # only allow editing availability or direction
    if (col_name %in% c("availability","direction")) {
      rv$resources[info$row, info$col] <-
        DT::coerceValue(info$value, rv$resources[info$row, info$col])
      # redraw the table immediately so the change shows up
      DT::replaceData(resources_proxy, rv$resources, resetPaging = FALSE)
      # clear any old solution and re-solve if we've already hit Solve once
      rv$solution <- NULL
      if (input$solve > 0) solve_model()
    }
  })

  observeEvent(input$activities_table_cell_edit, {
    info     <- input$activities_table_cell_edit
    col_name <- names(rv$activities)[ info$col ]
    # allow everything except the activity name itself
    if (col_name != "activity") {
      rv$activities[info$row, info$col] <-
        DT::coerceValue(info$value, rv$activities[info$row, info$col])
      # immediately reflect edit in the UI
      DT::replaceData(activities_proxy, rv$activities, resetPaging = FALSE)
      # clear any old solution and re-solve if needed
      rv$solution <- NULL
      if (input$solve > 0) solve_model()
    }
  })

  # Download handlers
  # Resources table CSV
  output$download_resources_csv <- downloadHandler(
    filename = "resources_table.csv",
    content = function(file) {
      write.csv(rv$resources, file, row.names = FALSE)
    }
  )

  # Activities table CSV
  output$download_activities_csv <- downloadHandler(
    filename = "activities_table.csv",
    content = function(file) {
      write.csv(rv$activities, file, row.names = FALSE)
    }
  )

  # --- Build & immediately solve RAM when user clicks Solve ---
  # extract the solve logic to a function we can call from multiple places
  solve_model <- function() {
    req(nrow(rv$resources) > 0, nrow(rv$activities) > 0)
    res <- define_resources(
      resources    = rv$resources$resource,
      availability = as.numeric(rv$resources$availability),
      direction    = rv$resources$direction
    )
    acts     <- rv$activities
    techCols <- setdiff(names(acts), c("activity","objective"))
    techMat  <- t(as.matrix(acts[, techCols]))
    colnames(techMat) <- acts$activity
    rownames(techMat) <- techCols
    actsDef <- define_activities(
      activities                   = acts$activity,
      activity_requirements_matrix = techMat,
      objective                    = as.numeric(acts$objective)
    )
    model       <- create_ram_model(res, actsDef)
    rv$solution <- solve_ram(model, direction = input$direction)
  }

  # Call solve_model() when button is clicked
  # CORRECT: only call solve_model() when solve is clicked
  observeEvent(input$solve, {
    solve_model()
  })


  # --- Outputs: enriched solution table with per‐activity resource use + totals ---
  output$solution_tbl <- DT::renderDT({
    req(rv$solution)
    sol <- rv$solution$optimal_activities
    df  <- data.frame(Activity = names(sol), Level = as.numeric(sol), stringsAsFactors = FALSE)

    # Identify resource columns dynamically, excluding *_min and *_max columns
    resource_cols <- setdiff(names(rv$activities), c("activity", "objective"))
    resource_cols <- resource_cols[!grepl("_min|_max", resource_cols)]

    # Match rows and extract resource usage
    reqs <- rv$activities[match(df$Activity, rv$activities$activity), resource_cols, drop = FALSE]

    # Multiply each resource column by the activity level
    for (res in resource_cols) {
      df[[paste0(res, "_used")]] <- df$Level * reqs[[res]]
    }

    # Create total row
    total_row <- as.list(c("Total", sum(df$Level), sapply(df[ , -(1:2)], sum)))
    names(total_row) <- names(df)
    df_final <- rbind(df, total_row)

    # Format column names
    col_names <- c("Activity", "Optimal Level", gsub("_", " ", tools::toTitleCase(resource_cols)))

    # Render table
    DT::datatable(df_final, rownames = FALSE, colnames = col_names, options = list(dom = "t")) |>
      DT::formatRound(columns = names(df_final)[-1], digits = 1)
  })

  output$objective_val <- renderPrint({
    req(rv$solution)
    rv$solution$objective_value
  })
  output$activity_plot <- renderPlotly({
    req(rv$solution)
    df <- data.frame(
      activity = names(rv$solution$optimal_activities),
      level    = as.numeric(rv$solution$optimal_activities)
    )
    plot_ly(df, x=~activity, y=~level, type="bar", marker=list(color="skyblue")) %>%
      layout(
        title="Optimal Resource Allocation",
        xaxis=list(title="Activity"),
        yaxis=list(title="Level")
      )
  })
  output$sensitivity_tbl <- renderDT({
    req(rv$solution)
    out <- tryCatch(sensitivity_ram(rv$solution), error = function(e) NULL)
    if (is.null(out)) {
      datatable(data.frame(Message = "No sensitivity available"), rownames = FALSE)
    } else {
      datatable(out, rownames = FALSE)
    }
  })

}
