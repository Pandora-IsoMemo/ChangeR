#' UI function of data module
#'
#' @param id module id
#'
#' @export
dataUI <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarPanel(
      ## left sidebar ----
      width = 2,
      style = "position:fixed; width:15%; max-width:350px; overflow-y:auto; height:85%",
      HTML("<h5>Upload of datasets</h5>"),
      DataTools::importDataUI(ns("file_data"), "Data"),
      tags$br(), tags$br(),
      HTML("<h5>Generate Data</h5>"),
      actionButton(ns("example_data"), "Load Example Data")
    ),
    mainPanel(
      ## main panel ----
      DT::dataTableOutput(ns("loadedData"))
    )
  )
}

#' Server function of data module
#'
#' @param id module id
#' @export
dataServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      data <- reactiveValues(dat = data.frame(),
                             results = NULL,
                             exportData = data.frame())

      output$loadedData <- DT::renderDataTable(data$dat)

      # UPLOAD DATA ----
      importedData <- importDataServer(
        "file_data",
        defaultSource = config()[["defaultSourceData"]],
        ckanFileTypes = config()[["ckanFileTypes"]],
        options = DataTools::importOptions(rPackageName = config()[["rPackageName"]])
      )

      observe({
        req(length(importedData()) > 0)
        data$dat <- importedData()[[1]]
      }) %>%
        bindEvent(importedData())

      # Load Example Data ----
      observe({
        # AirPassengers: load a classic time series dataset with a clear seasonal pattern and potential trend changes
        # Convert AirPassengers time series into a data frame
        data$dat <- data.frame(
          Month = as.Date(paste0(format(as.yearmon(time(AirPassengers)), "%Y-%m"), "-01")),
          Passengers = as.numeric(AirPassengers)
        )

      }) %>%
        bindEvent(input$example_data)

      return(reactive(data$dat))
    })
}
