#' Change Points UI
#'
#' UI of the module
#'
#' @rdname changePointsServer
changePointsUI <- function(id) {
  ns <- NS(id)
  tagList(tags$br(),
          tabsetPanel(
            id = ns("changePointsTabs"),
            tabPanel("Data", DT::dataTableOutput(ns("loadedData"))),
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
#' @param file_data The reactive file data
changePointsServer <- function(id, file_data) {
  moduleServer(id, function(input, output, session) {

    output$loadedData <- DT::renderDataTable(file_data$mainData)

    mcpData <- mcpDataServer(id = "mcpData", reactive(file_data$mainData))
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
