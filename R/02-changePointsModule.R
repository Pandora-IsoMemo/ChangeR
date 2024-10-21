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
            tabPanel("Data", tagList(
              tags$br(),
              DT::dataTableOutput(ns("data"))
            )),
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

    output$data <- DT::renderDataTable({
      validate(need(length(input_data$mainData) > 0, "Please load data first ..."))

      input_data$mainData
    })

    input_data$mcpData <- mcpDataServer(id = "mcpData", reactive(input_data$mainData))

    formulasList <- mcpFormulasServer(
      id = "formulas",
      input_data = input_data#,
      #uploadedSegments = input_data$uploadedSegments,
      #uploadedPriors = input_data$uploadedPriors
    )
    #input_data$segmentsMatrix <- formulasList$segmentsMatrix
    #input_data$priorsMatrix <- formulasList$priorsMatrix

    mcpFitList <- mcpModelingServer(id = "mcp",
                                    mcpData = input_data$mcpData,
                                    formulasAndPriors = formulasList$formulasAndPriors)

    mcpShowSingleModelServer(
      id = "singleModelOut",
      mcpData = input_data$mcpData,
      formulasAndPriors = formulasList$formulasAndPriors,
      mcpFitList = mcpFitList
    )

    mcpCompareModelsServer(
      id = "compareModelsOut",
      mcpData = input_data$mcpData,
      formulasAndPriors = formulasList$formulasAndPriors,
      mcpFitList = mcpFitList
    )

    return(mcpFitList)
  })
}
