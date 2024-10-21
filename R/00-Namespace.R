#' @rawNamespace import(shiny, except = c(renderDataTable, dataTableOutput))
#' @importFrom DataTools dataServer dataUI downloadModelServer downloadModelUI importOptions importServer importUI resetData
#' @importFrom DT dataTableOutput renderDataTable
#' @importFrom loo loo loo_compare waic
#' @importFrom magrittr "%>%"
#' @importFrom mcp mcp
#' @importFrom shinycssloaders withSpinner
#' @importFrom shinyTools shinyTryCatch updateUserInputs
#' @importFrom stats as.formula na.omit time
#' @importFrom utils read.csv
#' @importFrom yaml read_yaml
#' @importFrom zoo as.yearmon
#'
globalVariables(c("AirPassengers"))
NULL
