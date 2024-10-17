#' @rawNamespace import(shiny, except = c(renderDataTable, dataTableOutput))
#' @importFrom DataTools downloadModelServer downloadModelUI extractNotes extractObjectFromFile
#'  importDataUI importDataServer importOptions
#' @importFrom DT dataTableOutput renderDataTable
#' @importFrom loo loo loo_compare waic
#' @importFrom magrittr "%>%"
#' @importFrom mcp mcp
#' @importFrom shinycssloaders withSpinner
#' @importFrom shinyTools shinyTryCatch
#' @importFrom stats as.formula na.omit time
#' @importFrom utils read.csv
#' @importFrom yaml read_yaml
#' @importFrom zoo as.yearmon
#'
globalVariables(c("AirPassengers"))
NULL
