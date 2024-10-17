#' Change Points UI
#'
#' UI of the module
#'
#' @rdname changePointsServer
#'
#' @export
changePointsUI <- function(id) {
  ns <- NS(id)
  tagList(tags$br(),
          tabsetPanel(
            id = ns("changePointsTabs"),
            tabPanel("Data", DT::dataTableOutput(ns("data"))),
            tabPanel("MCP Lists from Segments & Priors", mcpFormulasUI(ns("formulas"))),
            tabPanel(
              "MCP Modeling",
              mcpDataUI(ns("mcpData")),
              mcpModelingUI(ns("mcp")),
              mcpShowSingleModelUI(ns("singleModelOut"))
            ),
            tabPanel("Comparison of Models", mcpCompareModelsUI(ns(
              "compareModelsOut"
            )))
          ),
          tags$br())
}

#' Change Points Server
#'
#' Server function of the module
#'
#' @param id The module id
#' @param input_data The reactive input data
#'
#' @export
changePointsServer <- function(id, input_data) {
  moduleServer(id, function(input, output, session) {

    output$data <- DT::renderDataTable(input_data$mainData)

    mcpData <- mcpDataServer(id = "mcpData", reactive(input_data$mainData))
    formulasAndPriors <- mcpFormulasServer(id = "formulas")
    mcpFitList <- mcpModelingServer(id = "mcp",
                                    mcpData = mcpData,
                                    formulasAndPriors = formulasAndPriors)

    mcpShowSingleModelServer(
      id = "singleModelOut",
      mcpData = mcpData,
      formulasAndPriors = formulasAndPriors,
      mcpFitList = mcpFitList
    )

    mcpCompareModelsServer(
      id = "compareModelsOut",
      mcpData = mcpData,
      formulasAndPriors = formulasAndPriors,
      mcpFitList = mcpFitList
    )
  })
}
