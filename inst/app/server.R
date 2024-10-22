shinyServer(function(input, output, session) {
  options(shiny.maxRequestSize = 400*1024^2)

  uploaded_notes <- reactiveVal(NULL)
  uploaded_inputs <- reactiveVal(NULL)
  uploaded_matrices <- reactiveValues()

  file_data <- DataTools::dataServer(
    id = "data",
    path = file.path("data", "example_breakPoints.csv"),
    defaultSource = config()[["defaultSourceData"]],
    ckanFileTypes = config()[["ckanFileTypes"]],
    options = DataTools::importOptions(rPackageName = config()[["rPackageName"]])
  )

  mcpFitList <- changePointsServer("changePoints", file_data, uploaded_matrices)

  ## Down- / Upload Session ----
  DataTools::downloadModelServer("modelDownload",
                                 dat = reactive(reactiveValuesToList(file_data)),
                                 inputs = input,
                                 model = mcpFitList,
                                 rPackageName = config()[["rPackageName"]],
                                 fileExtension = config()[["fileExtension"]],
                                 modelNotes = uploaded_notes,
                                 triggerUpdate = reactive(TRUE))

  uploadedValues <- DataTools::importServer("modelUpload",
                                            title = "Import Model",
                                            importType = "model",
                                            ckanFileTypes = config()[["ckanModelTypes"]],
                                            ignoreWarnings = TRUE,
                                            defaultSource = config()[["defaultSourceModel"]],
                                            fileExtension = config()[["fileExtension"]],
                                            options = DataTools::importOptions(
                                              rPackageName = config()[["rPackageName"]]
                                            ))

  observe({
    req(length(uploadedValues()) > 0)

    # update notes in tab down-/upload ----
    uploaded_notes(uploadedValues()[[1]][["notes"]])

    # load data
    file_data <- file_data %>%
      DataTools::resetData()

    for (entry in names(uploadedValues()[[1]][["data"]])) {
      file_data[[entry]] <- uploadedValues()[[1]][["data"]][[entry]]
    }

    # extract model object(s)
    mcpFitList(uploadedValues()[[1]][["model"]])

    # update user inputs
    uploaded_inputs(uploadedValues()[[1]][["inputs"]])
    uploaded_matrices[["segments"]] <- file_data[["segmentsMatrix"]]
    uploaded_matrices[["priors"]] <- file_data[["priorsMatrix"]]
  }) %>%
    bindEvent(uploadedValues())

  observe({
    req(!is.null(uploaded_inputs()))
    logDebug("server: Sending uploaded_inputs.")

    shinyTools::updateUserInputs(
      input = input,
      output = output,
      session = session,
      userInputs = uploaded_inputs()[["inputs"]]
    )
  }) %>% bindEvent(uploaded_inputs())

})
