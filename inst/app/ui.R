library(shiny)

tagList(
  navbarPage(
    title = paste("ChangeR", packageVersion("ChangeR")),
    theme = shinythemes::shinytheme("flatly"),
    position = "fixed-top",
    collapsible = TRUE,
    id = "tab",
    tabPanel(
      "Change Point Detection",
      fluidPage(
        shinyTools::includeShinyToolsCSS(),
        sidebarLayout(
          sidebarPanel(
            ## left sidebar ----
            width = 2,
            style = "position:fixed; width:15%; max-width:350px; overflow-y:auto; height:85%",
            # import of data
            DataTools::dataUI(id = "data", title = "Data"),
            # import and downloadof session
            DataTools::importUI("modelUpload", label = "Import Model", title = "Session"),
            DataTools::downloadModelUI("modelDownload", label = "Download Model") # download of session
          ),
          mainPanel(## main panel ----
                    changePointsUI("changePoints"))
        )
      )
    )
  ),
  shinyTools::headerButtonsUI(id = "header", help_link = "https://pandora-isomemo.github.io/ChangeR/"),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),
  shinyjs::useShinyjs()
)
