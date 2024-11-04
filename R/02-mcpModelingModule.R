# 2. MCP Modeling ----

#' UI function of mcpData module
#'
#' @rdname mcpDataServer
mcpDataUI <- function(id) {
  ns <- NS(id)
  tagList(tags$br(), tags$h4("MCP Data"), fluidRow(column(
    3, selectInput(ns("x"), "x column", choices = c("Please load data first ..." = ""))
  ), column(
    3, selectInput(ns("y"), "y column", choices = c("Please load data first ..." = ""))
  )))
}

#' Server function of mcpData module
#'
#' @param id module id
#' @param file_data reactive file data
#' @param mcp_columns (character) a character vector containing the default x and y columns for MCP modelling.
#' The first entry corresponds to the x column name, and the second entry corresponds to the y column name.
mcpDataServer <- function(id, file_data, mcp_columns = c(x = "", y = "")) {
  moduleServer(id, function(input, output, session) {
    colnames_file_data <- reactiveVal()
    mcpData <- reactiveVal()

    observe({
      logDebug("%s: Entering observe 'file_data()' ...", id)

      # reset if file_data is empty
      if (length(file_data()) == 0 || nrow(file_data()) == 0 || ncol(file_data()) == 0) {
        colnames_file_data(NULL)
        # reset inputs
        updateSelectInput(session, "x", choices = c("Please load data first ..." = ""))
        updateSelectInput(session, "y", choices = c("Please load data first ..." = ""))
        mcpData(NULL)
      }

      req(nrow(file_data()) > 0, ncol(file_data()) > 0)
      # update colnames if new
      if (is.null(colnames_file_data()) ||
          !identical(colnames_file_data(), colnames(file_data()))) {
        colnames_file_data(colnames(file_data()))

        # set selected value
        selected_x <- ifelse(
          (length(mcp_columns) > 0) && !is.null(mcp_columns[["x"]]) && mcp_columns[["x"]] %in% colnames_file_data(),
          mcp_columns[["x"]],
          colnames_file_data()[1]
        )
        # update x input
        updateSelectInput(session,
                          "x",
                          choices = colnames_file_data(),
                          selected = selected_x)

        if (length(colnames_file_data()) == 0) {
          updateSelectInput(session, "y", choices = c("Data has only one column ..." = ""))
        } else {
          # set selected value
          selected_y <- ifelse(
            (length(mcp_columns) > 0) && !is.null(mcp_columns[["y"]]) && mcp_columns[["y"]] %in% colnames_file_data(),
            mcp_columns[["y"]],
            colnames_file_data()[2]
          )
          # update y input
          updateSelectInput(session,
                            "y",
                            choices = colnames_file_data(),
                            selected = selected_y)
      }
      }

    }) %>% bindEvent(file_data(), ignoreNULL = FALSE)

    observe({
      logDebug("%s: Entering observe 'input$x', 'input$y' ...", id)

      req(input[["x"]], input[["y"]])
      # select relevant columns
      newData <- file_data()[, c(input[["x"]], input[["y"]])]
      # x column as numeric
      newData[[input[["x"]]]] <- as.numeric(newData[[input[["x"]]]])
      # rename columns
      colnames(newData) <- c("x", "y")
      # remove rows with NA
      newData <- na.omit(newData)

      mcpData(newData)
    }) %>% bindEvent(input[["x"]], input[["y"]])

    return(mcpData)
  })
}


#' MCP Modeling UI
#'
#' @rdname mcpModelingServer
mcpModelingUI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$br(),
    tags$h4("MCP Run"),
    fluidRow(
      column(
        3,
        numericInput(ns("adapt"), "Burn in length", value = 5000),
        helpText("Increase for better conversion (makes the run slower).")
      ),
      column(3, numericInput(
        ns("chains"),
        "Number of chains",
        value = 3,
        min = 1
      )),
      column(
        3,
        numericInput(
          ns("iter"),
          "Number of iterations",
          value = 3000,
          min = 1
        )
      ),
      column(
        3,
        align = "right",
        style = "margin-top: 1.75em;",
        # load example data instead of plot data:
        #actionButton(ns("loadExampleDf"), "Load Example Data"),
        actionButton(ns("apply"), "Run", disabled = TRUE)
      )
    ),
    tags$hr()
  )
}

#' MCP Modeling Server
#'
#' @inheritParams mcpOutServer
mcpModelingServer <- function(id, formulasAndPriors, mcpData) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    mcpFitList <- reactiveVal()

    observe({
      req(formulasAndPriors(), mcpData())
      logDebug("%s: Enable 'input$apply' ...", id)

      # enable the 'Run Model' button
      shinyjs::enable(ns("apply"), asis = TRUE) # use this instead of updateActionButton
      #updateActionButton(session, "apply", disabled = FALSE) # not working with current version in docker
    }) %>% bindEvent(formulasAndPriors(), mcpData())

    observe({
      logDebug("%s: Entering observe 'input$apply' ...", id)

      res <- runMcp(
        lists = formulasAndPriors(),
        data = mcpData(),
        adapt = input[["adapt"]],
        chains = input[["chains"]],
        iter = input[["iter"]]
      ) %>%
        shinyTryCatch(errorTitle = "Error in fitting mcp model", warningTitle = "Warning in fitting mcp model") %>%
        withProgress(message = "Fitting MCP model...", value = 0.5)

      mcpFitList(res)
    }) %>%
      bindEvent(input[["apply"]])

    return(mcpFitList)
  })
}

#' MCP Show Single Model UI
#'
#' @rdname mcpShowSingleModelServer
mcpShowSingleModelUI <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(
      ns("showModel"),
      "Show MCP model",
      choices = c("'Run MCP' first ..." = "")
    ),
    tags$br(),
    fluidRow(column(
      6,
      mcpOutUI(
        id = ns("summary"),
        title = "Model Summary",
        outFUN = verbatimTextOutput,
        showWidth = TRUE
      )
    ), column(
      6,
      mcpOutUI(
        id = ns("waic"),
        title = "Model WAIC",
        outFUN = verbatimTextOutput
      )
    )),
    mcpOutUI(
      id = ns("plot"),
      title = "Model Plot",
      outFUN = plotOutput
    )
  )
}

#' MCP Show Single Model Server
#'
#' @inheritParams mcpOutServer
mcpShowSingleModelServer <- function(id,
                                     mcpData,
                                     formulasAndPriors,
                                     mcpFitList) {
  moduleServer(id, function(input, output, session) {
    observe({
      logDebug("%s: Entering observe 'mcpFitList()' ...", id)

      mcpModels <- seq_along(mcpFitList())
      names(mcpModels) <- paste("Model", mcpModels)
      if (length(mcpModels) > 0) {
        mcpSelected <- 1
      } else {
        mcpSelected <- NULL
      }

      updateSelectInput(session,
                        "showModel",
                        choices = mcpModels,
                        selected = mcpSelected)
    }) %>%
      bindEvent(mcpFitList())

    mcpOutServer(
      id = "summary",
      formulasAndPriors = formulasAndPriors,
      mcpData = mcpData,
      mcpFitList = mcpFitList,
      mcpModelName = reactive(input[["showModel"]]),
      mcpOutFUN = "summary",
      renderFUN = renderPrint
    )

    mcpOutServer(
      id = "waic",
      formulasAndPriors = formulasAndPriors,
      mcpData = mcpData,
      mcpFitList = mcpFitList,
      mcpModelName = reactive(input[["showModel"]]),
      mcpOutFUN = "waic",
      renderFUN = renderPrint
    )

    mcpOutServer(
      id = "plot",
      formulasAndPriors = formulasAndPriors,
      mcpData = mcpData,
      mcpFitList = mcpFitList,
      mcpModelName = reactive(input[["showModel"]]),
      mcpOutFUN = "plot",
      renderFUN = renderPlot
    )
  })
}

#' MCP Output UI
#'
#' @param title The title of the output
#' @param outFUN The output function, either verbatimTextOutput or plotOutput
#' @param showWidth Show the width slider
#' @rdname mcpOutServer
mcpOutUI <- function(id,
                     title,
                     outFUN = verbatimTextOutput,
                     showWidth = FALSE) {
  ns <- NS(id)

  tagList(
    fluidRow(column(6, tags$h4(title)), column(6, align = "right", uiOutput(
      ns("exportButton")
    ))),
    tags$br(),
    outFUN(ns("modelOut")) %>% withSpinner(color = "#20c997"),
    if (showWidth) {
      sliderInput(
        ns("width"),
        "Summary width",
        min = 0.01,
        max = 0.99,
        value = 0.95,
        step = 0.01,
        width = "100%"
      )
    },
    tags$br()
  )
}

#' MCP Output Server
#'
#' @param id The module id
#' @param formulasAndPriors The reactive formulas and priors
#' @param mcpData The reactive mcp data
#' @param mcpFitList The reactive mcp fit list
#' @param mcpModelName The reactive mcp model name
#' @param mcpOutFUN The output function, either "summary", "waic" or "plot"
#' @param renderFUN The render function, either renderPrint or renderPlot
mcpOutServer <- function(id,
                         formulasAndPriors,
                         mcpData,
                         mcpFitList,
                         mcpModelName,
                         mcpOutFUN,
                         renderFUN = renderPrint) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    mcpModel <- reactiveVal()

    observe({
      req(mcpFitList(), mcpModelName())
      logDebug("%s: Entering observe 'input$showModel' ...", id)

      res <- mcpFitList()[[as.numeric(mcpModelName())]]
      mcpModel(res)
    }) %>% bindEvent(mcpModelName())

    mcpOutFUNParams <- reactive({
      ifelse(is.null(input[["width"]]), list(), list(width = input[["width"]]))
    })

    # displayed content (either print or plot)
    out_content <- reactive({
      if (is.null(mcpModel()))
        return(NULL)

      do.call(mcpOutFUN, c(list(mcpModel()), mcpOutFUNParams())) %>%
        shinyTryCatch(
          errorTitle = sprintf("Error during creating '%s' output", id),
          warningTitle = sprintf("Warning during creating '%s' output", id)
        )
    })

    output$modelOut <- renderFUN({
      validate(need(
        formulasAndPriors(),
        "Please 'Create MCP Lists' and 'Run MCP' first ..."
      ))
      validate(need(mcpData(), "Please load data and 'Run MCP' first ..."))
      validate(need(mcpFitList(), "Please 'Run MCP' first ..."))
      validate(need(mcpModel(), "Please select MCP model first ..."))

      out_content()
    })

    output$exportButton <- renderUI({
      if (mcpOutFUN %in% c("plot")) {
        plotExportButton(ns("download"), "Export")
      } else {
        textExportButton(ns("download"), "Export")
      }
    })

    if (mcpOutFUN %in% c("plot")) {
      plotExportServer("download",
                       filename = sprintf("model_%s_%s", mcpModelName(), mcpOutFUN),
                       plotFun = reactive({
        function() {
          if (is.null(mcpModel()))
            return(NULL)

          # cannot use out_content() directly, because some output content gets lost
          #out_content() # !NOT USE!
          do.call(mcpOutFUN, c(list(mcpModel()), mcpOutFUNParams())) %>%
            shinyTryCatch(
              errorTitle = sprintf("Error during creating '%s' output", id),
              warningTitle = sprintf("Warning during creating '%s' output", id)
            )
        }
      }))
    } else {
      textExportServer("download",
                       filename = sprintf("model_%s_%s", mcpModelName(), mcpOutFUN),
                       outFun = reactive({
                         function() {
                               if (is.null(mcpModel()))
                                 return(NULL)

                               # cannot use out_content() directly, because some output content gets lost
                               #out_content() # !NOT USE!
                               do.call(mcpOutFUN, c(list(mcpModel()), mcpOutFUNParams())) %>%
                                 shinyTryCatch(
                                   errorTitle = sprintf("Error during creating '%s' output", id),
                                   warningTitle = sprintf("Warning during creating '%s' output", id)
                                 )
                         }
                       }))
    }
  })
}
