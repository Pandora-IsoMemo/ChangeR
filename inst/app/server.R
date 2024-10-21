shinyServer(function(input, output, session) {
  options(shiny.maxRequestSize = 400*1024^2)

  file_data <- DataTools::dataServer(
    id = "data",
    path = file.path("data", "example_breakPoints.csv"),
    defaultSource = config()[["defaultSourceData"]],
    ckanFileTypes = config()[["ckanFileTypes"]],
    options = DataTools::importOptions(rPackageName = config()[["rPackageName"]])
  )

  mcpFitList <- changePointsServer("changePoints", file_data)

  ## Down- / Upload Session ----
  uploadedNotes <- reactiveVal(NULL)
  uploadedInputs <- reactiveVal(NULL)

  DataTools::downloadModelServer("modelDownload",
                                 dat = reactive(reactiveValuesToList(file_data)),
                                 inputs = input,
                                 model = mcpFitList,
                                 rPackageName = config()[["rPackageName"]],
                                 fileExtension = config()[["fileExtension"]],
                                 modelNotes = uploadedNotes,
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
    uploadedNotes(uploadedValues()[[1]][["notes"]])

    # load data
    file_data <- file_data %>%
      DataTools::resetData()

    for (entry in names(uploadedValues()[[1]][["data"]])) {
      file_data[[entry]] <- uploadedValues()[[1]][["data"]][[entry]]
    }

    # extract model object(s)

    mcpFitList(uploadedValues()[[1]][["model"]])

    uploadedInputs(uploadedValues()[[1]][["inputs"]])
  }) %>%
    bindEvent(uploadedValues())

  observe({
    req(!is.null(uploadedInputs()))
    logDebug("server: Sending uploadedInputs.")

    shinyTools::updateUserInputs(
      input = input,
      output = output,
      session = session,
      userInputs = uploadedInputs()[["inputs"]]
    )
  }) %>% bindEvent(uploadedInputs())

})
