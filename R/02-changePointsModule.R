#' Change Points UI
#'
#' UI of the module
#'
#' @rdname changePointsServer
#'
#' @export
changePointsUI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$br(),
    tabsetPanel(
      id = ns("changePointsTabs"),
      tabPanel("Data", tagList(tags$br(), DT::dataTableOutput(ns(
        "data"
      )))),
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
    tags$br()
  )
}

#' Change Points Server
#'
#' Server function of the module
#'
#' @param id The module id
#' @param file_data The reactive input data
#' @inheritParams mcpFormulasServer
#'
#' @export
changePointsServer <- function(id, file_data, uploaded_matrices) {
  moduleServer(id, function(input, output, session) {
    # render full input data
    output$data <- DT::renderDataTable({
      validate(need(
        length(file_data$mainData) > 0,
        "Please load data first ..."
      ))

      file_data$mainData
    })

    # MCP Data ----
    # select columns from input data
    mcpData <- mcpDataServer(id = "mcpData", reactive(file_data$mainData))

    # keep track of the mcpData
    observe({
      logDebug(sprintf("%s: Entering observe 'mcpData()'", id))
      file_data$mcpData <- mcpData()
    }) %>%
      bindEvent(mcpData(), ignoreInit = TRUE)

    # MCP Segments & Priors ----
    formulasList <- mcpFormulasServer(id = "formulas", uploaded_matrices = uploaded_matrices)

    # keep track of the segments and priors matrices (user inputs)
    observe({
      logDebug(sprintf(
        "%s: Entering observe 'formulasList$segmentsMatrix()'",
        id
      ))
      file_data$segmentsMatrix <- formulasList$segmentsMatrix()
    }) %>%
      bindEvent(formulasList$segmentsMatrix(), ignoreInit = TRUE)

    observe({
      logDebug(sprintf("%s: Entering observe 'formulasList$priorsMatrix()'", id))
      file_data$priorsMatrix <- formulasList$priorsMatrix()
    }) %>%
      bindEvent(formulasList$priorsMatrix(), ignoreInit = TRUE)

    # MCP Modeling ----
    mcpFitList <- mcpModelingServer(
      id = "mcp",
      mcpData = mcpData,
      formulasAndPriors = formulasList$formulasAndPriors
    )

    # MCP Output ----
    # Show single model output
    mcpShowSingleModelServer(
      id = "singleModelOut",
      mcpData = mcpData,
      formulasAndPriors = formulasList$formulasAndPriors,
      mcpFitList = mcpFitList
    )

    # Compare models
    mcpCompareModelsServer(
      id = "compareModelsOut",
      mcpData = mcpData,
      formulasAndPriors = formulasList$formulasAndPriors,
      mcpFitList = mcpFitList
    )

    return(mcpFitList)
  })
}
