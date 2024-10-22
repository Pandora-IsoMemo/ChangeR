# 1. MCP Segments & Priors ----

#' MCP Formulas UI
#'
#' @rdname mcpFormulasServer
mcpFormulasUI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$br(),
    fluidRow(column(8, tags$h4("MCP Segments")), column(
      4, align = "right", infoButtonUI(ns("formulaInfo"), label = "Segment Info")
    )),
    matrixUI(
      ns("segments"),
      title = NULL,
      defaultCellContent = "y ~ 1 + x",
      exampleLabel = "Example Segments"
    ),
    tags$br(),
    fluidRow(column(8, tags$h4("MCP Priors")), column(
      4, align = "right", infoButtonUI(ns("PriorInfo"), label = "Prior Info")
    )),
    matrixUI(
      ns("priors"),
      title = NULL,
      defaultCellContent = "x_1 = dunif(-4, -0.5);",
      exampleLabel = "Example Priors"
    ),
    tags$hr(),
    verbatimTextOutput(ns("mcpFormulas"))
  )
}

#' MCP Formulas Server
#'
#' @param id The module id
#' @param uploaded_matrices (reactive) values of uploaded segments and priors matrices
mcpFormulasServer <- function(id, uploaded_matrices) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    formulasList <- reactiveValues()
    formulasAndPriors <- reactiveVal()

    formulasList$segmentsMatrix <- matrixServer(
      "segments",
      exampleFunction = readExampleMatrix,
      validateCellFunction = validateFormula,
      path = file.path("data", "example_breakPointSegments.csv"),
      uploadedMatrix = reactive(uploaded_matrices$segments)
    )
    formulasList$priorsMatrix <- matrixServer(
      "priors",
      exampleFunction = readExampleMatrix,
      path = file.path("data", "example_breakPointPriors.csv"),
      uploadedMatrix = reactive(uploaded_matrices$priors)
    )

    infoButtonServer(
      id = "formulaInfo",
      title = "'Formula' (segment) description",
      text = "The 'formula' argument in mcp defines the model structure. It specifies the relationship between
        variables and the change points in your data. The formula should be written similarly to formulas
        in base R, with additional support for change points.",
      link = "https://lindeloev.github.io/mcp/articles/formulas.html"
    )

    infoButtonServer(
      id = "PriorInfo",
      title = "'Prior' description",
      text = "The 'prior' argument in mcp specifies the prior distributions for the parameters in your model.
        It should be a named list where the names correspond to the model parameters,
        and the values are prior distributions in JAGS notation. If you don't specify a prior for a parameter,
        mcp will use a default weakly informative prior.",
      link = "https://lindeloev.github.io/mcp/articles/priors.html"
    )

    observe({
      req(formulasList$segmentsMatrix(),
          formulasList$priorsMatrix())

      logDebug("%s: Entering observe 'segmentsMatrix()', 'priorsMatrix()' ...",
               id)
      newFormulasAndPriors <- getComb(segments = formulasList$segmentsMatrix(),
                                      priors = formulasList$priorsMatrix()) %>%
        cleanComb() %>%
        splitComb() %>%
        setFormulasAndPriors() %>%
        shinyTryCatch(errorTitle = "Error in creating MCP lists", warningTitle = "Warning in creating MCP lists")

      formulasAndPriors(newFormulasAndPriors)
    }) %>% bindEvent(list(
      formulasList$segmentsMatrix(),
      formulasList$priorsMatrix()
    ),
    ignoreInit = TRUE)

    formulasList$formulasAndPriors <- reactive({
      req(formulasAndPriors())
      formulasAndPriors()
    })

    output$mcpFormulas <- renderPrint({
      validate(need(
        formulasAndPriors(),
        "Please 'Set' Segments and Priors first ..."
      ))
      formulasAndPriors()
    })

    return(formulasList)
  })
}

#' Get Example Matrix
#'
#' @param path path to example matrix
readExampleMatrix <- function(path) {
  df <- read.csv(path)

  df[is.na(df)] <- ""

  res <- df %>% as.matrix()
  colnames(res) <- 1:ncol(res)
  rownames(res) <- 1:nrow(res)

  res
}

#' Info Button UI
#'
#' @param label The label of the button
#' @rdname infoButtonServer
infoButtonUI <- function(id, label = "Description") {
  ns <- NS(id)
  tagList(actionButton(ns("show_info"), icon = icon("info-circle"), label))
}

#' Info Button Server
#'
#' @param id The module id
#' @param title The title of the modal
#' @param text The text to display in the modal
#' @param link The link to the documentation
infoButtonServer <- function(id,
                             title = "Description",
                             text,
                             link = NULL) {
  moduleServer(id, function(input, output, session) {
    observe({
      showModal(
        modalDialog(
          title = title,
          p(text),
          # Add a hyperlink to the github help page
          if (!is.null(link)) {
            p(
              "For more details, visit the",
              a("documentation", href = link, target = "_blank"),
              "."
            )
          } else
            NULL,
          footer = modalButton("Close"),
          easyClose = TRUE
        )
      )
    }) %>%
      bindEvent(input$show_info)
  })
}
