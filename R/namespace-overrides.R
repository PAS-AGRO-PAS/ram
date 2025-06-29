# R/namespace-overrides.R

#' Namespace Overrides for DT - Shiny
#'
#' We want to use DT’s versions of the table-output functions,
#' not shiny’s. This tells R to import all of shiny except those.
#'
#' @rawNamespace import(shiny, except = c(dataTableOutput, renderDataTable))
#' @importFrom DT DTOutput renderDT
#' @name namespace-overrides
NULL

