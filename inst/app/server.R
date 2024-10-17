
shinyServer(function(input, output, session) {
  #options(shiny.maxRequestSize = 400*1024^2)

  file_data <- dataServer(id = "data")
  mcpData <- mcpDataServer(id = "mcpData", file_data)
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
