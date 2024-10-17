library(shiny)

tagList(
  navbarPage(
    header = shinyTools::includeShinyToolsCSS(),
    title = paste("ChangeR", packageVersion("ChangeR")),
    theme = shinythemes::shinytheme("flatly"),
    position = "fixed-top",
    collapsible = TRUE,
    id = "tab",
    tabPanel("Data", dataUI(id = "data")),
    tabPanel("MCP Lists from Segments & Priors", mcpFormulasUI("formulas")),
    tabPanel(
      "MCP Modeling",
      mcpDataUI("mcpData"),
      mcpModelingUI("mcp"),
      mcpShowSingleModelUI("singleModelOut")
    ),
    tabPanel("Comparison of Models", mcpCompareModelsUI(
      "compareModelsOut"
    ))
  ),
  shinyTools::headerButtonsUI(id = "header", help_link = ""),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),
  shinyjs::useShinyjs()
)
