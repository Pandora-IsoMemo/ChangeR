# 2. MCP Modeling ----

#' UI function of mcpData module
#'
#' @rdname mcpDataServer
mcpDataUI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$br(),
    tags$h4("MCP Data"),
    fluidRow(
      column(3,
             selectInput(
               ns("x"),
               "x column",
               choices = c("Please load data first ..." = "")
             )
             ),
      column(3,
             selectInput(
               ns("y"),
               "y column",
               choices = c("Please load data first ..." = "")
             )
             )
    )
  )
}

#' Server function of mcpData module
#'
#' @param id module id
#' @param file_data reactive file data
mcpDataServer <- function(id, file_data) {
  moduleServer(
    id,
    function(input, output, session) {
      mcpData <- reactiveVal()

      observe({
        req(nrow(file_data()) > 0, ncol(file_data()) > 0)
        logDebug("%s: Entering observe 'file_data()' ...", id)
        # update column select inputs
        updateSelectInput(session, "x", choices = colnames(file_data()))

        if (ncol(file_data()) == 1) {
          updateSelectInput(session, "y", choices = c("Data has only one column ..." = ""))
        } else {
          updateSelectInput(session, "y", choices = colnames(file_data()), selected = colnames(file_data())[2])
        }
      }) %>% bindEvent(file_data())

      observe({
        req(input[["x"]], input[["y"]])
        logDebug("%s: Entering observe 'input$x', 'input$y' ...", id)

        # select relevant columns
        newData <- file_data()[, c(input[["x"]], input[["y"]])]
        # x column as numeric
        newData[, input[["x"]]] <- as.numeric(newData[, input[["x"]]])
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
  tagList(tags$br(),
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
          tags$hr())
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
    ns <- session$ns
    mcpModel <- reactiveVal()

    observe({
      logDebug("%s: Entering observe 'mcpFitList()' ...", id)

      mcpModels <- seq_along(mcpFitList())
      names(mcpModels) <- paste("Model", mcpModels)
      if (length(mcpModels) > 0)
        mcpSelected <- 1
      else
        mcpSelected <- NULL
      updateSelectInput(session,
                        "showModel",
                        choices = mcpModels,
                        selected = mcpSelected)
    }) %>%
      bindEvent(mcpFitList())

    observe({
      req(mcpFitList(), input[["showModel"]])
      logDebug("%s: Entering observe 'input$showModel' ...", id)

      res <- mcpFitList()[[as.numeric(input[["showModel"]])]]
      mcpModel(res)
    }) %>% bindEvent(input[["showModel"]])

    mcpOutServer(
      id = "summary",
      formulasAndPriors = formulasAndPriors,
      mcpData = mcpData,
      mcpFitList = mcpFitList,
      mcpModel = mcpModel,
      outFUN = summary,
      renderFUN = renderPrint
    )

    mcpOutServer(
      id = "waic",
      formulasAndPriors = formulasAndPriors,
      mcpData = mcpData,
      mcpFitList = mcpFitList,
      mcpModel = mcpModel,
      outFUN = waic,
      renderFUN = renderPrint
    )

    mcpOutServer(
      id = "plot",
      formulasAndPriors = formulasAndPriors,
      mcpData = mcpData,
      mcpFitList = mcpFitList,
      mcpModel = mcpModel,
      outFUN = plot,
      renderFUN = renderPlot
    )
  })
}

#' MCP Output UI
#'
#' @param title The title of the output
#' @param showWidth Show the width slider
#' @rdname mcpOutServer
mcpOutUI <- function(id,
                     title,
                     outFUN = verbatimTextOutput,
                     showWidth = FALSE) {
  ns <- NS(id)

  tagList(tags$h4(title),
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
          })
}

#' MCP Output Server
#'
#' @param id The module id
#' @param formulasAndPriors The reactive formulas and priors
#' @param mcpData The reactive mcp data
#' @param mcpFitList The reactive mcp fit list
#' @param mcpModel The reactive mcp model
#' @param outFUN The output function
#' @param renderFUN The render function
mcpOutServer <- function(id,
                         formulasAndPriors,
                         mcpData,
                         mcpFitList,
                         mcpModel,
                         outFUN,
                         renderFUN = renderPrint) {
  moduleServer(id, function(input, output, session) {
    output$modelOut <- renderFUN({
      validate(need(
        formulasAndPriors(),
        "Please 'Create MCP Lists' and 'Run MCP' first ..."
      ))
      validate(need(mcpData(), "Please load data and 'Run MCP' first ..."))
      validate(need(mcpFitList(), "Please 'Run MCP' first ..."))
      validate(need(mcpModel(), "Please select MCP model first ..."))

      params <- ifelse(is.null(input[["width"]]), list(), list(width = input[["width"]]))

      do.call(outFUN, c(list(mcpModel()), params)) %>%
        shinyTryCatch(
          errorTitle = sprintf("Error during creating '%s' output", id),
          warningTitle = sprintf("Warning during creating '%s' output", id)
        )
    })
  })
}
