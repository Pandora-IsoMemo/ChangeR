# 3. Comparison of Models ----

#' MCP Compare Models UI
#'
#' @rdname mcpCompareModelsServer
mcpCompareModelsUI <- function(id) {
  ns <- NS(id)

  tagList(
    tags$br(),
    selectInput(ns("method"), "Method", c("loo", "waic", "heuristic")),
    tags$br(),
    verbatimTextOutput(ns("compareModels")) %>% withSpinner(color = "#20c997")
  )
}

#' MCP Compare Models Server
#'
#' @inheritParams mcpOutServer
mcpCompareModelsServer <- function(id,
                                   formulasAndPriors,
                                   mcpData,
                                   mcpFitList) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    compareModels <- reactiveVal()

    output$compareModels <- renderPrint({
      validate(need(
        formulasAndPriors(),
        "Please 'Create MCP Lists' and 'Run MCP' first ..."
      ))
      validate(need(mcpData(), "Please load 'Plot Data' and 'Run MCP' first ..."))
      validate(need(mcpFitList(), "Please 'Run MCP' first ..."))

      compareFUN <- switch(input[["method"]],
                           loo = compareWithLoo,
                           waic = compareWithWAIC,
                           heuristic = compareWithHeuristic)

      mcpFitList() %>%
        compareFUN() %>%
        shinyTryCatch(errorTitle = "Error in model comparison", warningTitle = "Warning in model comparison")
    })
  })
}
