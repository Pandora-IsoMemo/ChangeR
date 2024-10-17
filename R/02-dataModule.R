#' UI function of data module
#'
#' @rdname dataServer
#'
#' @export
dataUI <- function(id) {
  ns <- NS(id)
  tagList(
    DataTools::importDataUI(ns("file_data")),
    tags$br(), tags$br(),
    uiOutput(ns("exampleUI"))
  )
}

#' Server function of data module
#'
#' @param id module id
#' @param path path to the example data file, e.g. \code{file.path("data", "example.csv")}
#' @param transformations list of transformations to apply to the dataset
#'
#' @export
dataServer <- function(id, path = NULL, transformations = list()) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      data <- reactiveValues(mainData = data.frame(),            # Main dataset that will be used in subsequent modules
                             rawData = data.frame(),             # Original unmodified dataset
                             dataInfo = NULL,                    # Metadata about the dataset
                             source = NULL,                      # Source of the data (e.g., "upload", "example")
                             fileName = NULL,                    # Name of the uploaded file, if applicable
                             loadStatus = "pending",             # Status of data load
                             errorMessage = NULL,                # Error message for load failures
                             transformations = transformations   # List of transformations to apply to the dataset
      )

      # show example button if path is provided
      if (!is.null(path)) {
        output$exampleUI <- renderUI({
          actionButton(ns("example_data"), "Load Example Data")
        })

        observe({
          req(input$example_data)
          logDebug("%s: Clicked 'input$example_data' ...", id)

          data <- data %>% resetData()
          data$rawData <- path %>%
            read.csv() %>%
            shinyTryCatch(errorTitle = "Reading example file failed", alertStyle = "shinyalert")
          data$mainData <- data$rawData %>%
            applyTransformations(data$transformations) %>%
            shinyTryCatch(errorTitle = "Applying transformations to file failed", alertStyle = "shinyalert")
          data$dataInfo <- extractDataInfo(data$mainData)
          data$source <- "example"
          data$fileName <- basename(path)
          data$loadStatus <- "success"
        })
      } else {
        output$exampleUI <- NULL
      }

      # UPLOAD DATA ----
      importedData <- importDataServer(
        "file_data",
        defaultSource = config()[["defaultSourceData"]],
        ckanFileTypes = config()[["ckanFileTypes"]],
        options = DataTools::importOptions(rPackageName = config()[["rPackageName"]])
      )

      observe({
        req(length(importedData()) > 0)
        logDebug("%s: Updating 'importedData()' ...", id)

        data <- data %>% resetData()
        data$rawData <- importedData()[[1]]
        data$mainData <- data$rawData %>% applyTransformations(data$transformations) %>%
          shinyTryCatch(errorTitle = "Applying transformations to file failed", alertStyle = "shinyalert")
        data$dataInfo <- extractDataInfo(data$mainData)
        data$source <- "upload"
        data$fileName <- ""  # Replace with actual file name if available
        data$loadStatus <- "success"
      }) %>%
        bindEvent(importedData())

      return(data)
    })
}

resetData <- function(data) {
  # Reset entries in the `data` reactiveValues object
  data$mainData <- data.frame()       # Clear the main data
  data$rawData <- data.frame()        # Clear the raw data
  data$dataInfo <- NULL               # Clear metadata
  data$source <- NULL                 # Clear data source info
  data$fileName <- NULL               # Clear file name
  data$loadStatus <- "pending"        # Reset load status

  return(data)
}

applyTransformations <- function(rawData, transformations) {
  mainData <- rawData
  # Apply a list of transformations to the main data
  for (transformation in transformations) {
    mainData <- transformation(mainData)
  }

  return(mainData)
}

extractDataInfo <- function(mainData) {
  list(
    nrows = nrow(mainData),
    ncols = ncol(mainData),
    colnames = colnames(mainData)
  )
}
