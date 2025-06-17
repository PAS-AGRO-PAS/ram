# R/scenarios.R

#' Run a batch of resource‐allocation scenarios
#'
#' @param res_def A resources spec as returned by define_resources()
#' @param act_def An activities spec as returned by define_activities()
#' @param scenario_list A named list of vectors; each vector are the
#'        alternative values for one resource (names must match res_def$resource)
#' @param direction "max" or "min"
#' @return A data.frame with one row per scenario, the scenario inputs,
#'         the achieved objective, and one column per activity giving the
#'         optimal level of that activity.
#' @export
run_scenarios <- function(res_def, act_def, scenario_list, direction="max") {
  # 1) Expand into a data.frame of all combinations
  scen_grid <- expand.grid(scenario_list, stringsAsFactors = FALSE)
  
  # 2) For each row / scenario, rebuild resources, solve, and collect results
  out_list <- lapply(seq_len(nrow(scen_grid)), function(i) {
    scen <- scen_grid[i, , drop = FALSE]
    
    # a) modify the baseline resource availability
    avail <- res_def$availability
    names(avail) <- res_def$resource
    avail[names(scen)] <- unlist(scen)
    
    # b) rebuild the resource definition
    res2 <- define_resources(
      resources    = res_def$resource,
      availability = avail,
      direction    = res_def$direction
    )
    
    # c) solve the model
    mod  <- create_ram_model(res2, act_def)
    sol  <- solve_ram(mod, direction = direction)
    
    # d) gather the scalar objective
    obj  <- sol$objective_value
    
    # e) extract the named activity levels into a 1×N data.frame
    levels <- sol$optimal_activities
    df_lv  <- as.data.frame(t(levels), stringsAsFactors = FALSE)
    df_lv[] <- lapply(df_lv, as.numeric)
    
    # f) glue everything together in one data.frame, preserving names
    data.frame(
      scen,
      objective = obj,
      df_lv,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  })
  
  # 3) row‐bind all scenarios into one frame
  do.call(rbind, out_list)
}
