library(shiny)

tagList(
  navbarPage(
    header = shinyTools::includeShinyToolsCSS(),
    title = paste("ChangeR", packageVersion("ChangeR")),
    theme = shinythemes::shinytheme("flatly"),
    position = "fixed-top",
    collapsible = TRUE,
    id = "tab",
    sidebarPanel(
      ## left sidebar ----
      width = 2,
      style = "position:fixed; width:15%; max-width:350px; overflow-y:auto; height:85%",
      dataUI(id = "data")
    ),
    mainPanel(
      ## main panel ----
      changePointsUI("changePoints")
    )
  ),
  shinyTools::headerButtonsUI(id = "header", help_link = ""),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),
  shinyjs::useShinyjs()
)
