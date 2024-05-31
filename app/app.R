#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# library(shiny)
# library(shinythemes)
# # Define UI for application that draws a histogram
# ui <- fluidPage(
#   
#   includeCSS("stylesheet.css"),
#   shiny::fluidRow(
#     shinydashboard::box(
#       shiny::actionButton(
#         inputId = 'ab1',
#         label = "Need Help?",
#         icon = icon("th"),
#         onclick = "window.open('https://github.com/Pandora-IsoMemo/CausalR/blob/main/HELP.pdf', '_blank')",
#         style="color: #fff; background-color: #006c66; border-color: #2e6da4"
#       ),
#       shiny::actionButton(
#         inputId = 'ab2',
#         label = "How to format data before upload",
#         icon = icon("th"),
#         onclick = "window.open('https://github.com/Pandora-IsoMemo/CausalR/blob/main/HELP.pdf', '_blank')",
#         style="color: #fff; background-color: #006c66; border-color: #2e6da4"
#       )
#     )
#   ),
#   #theme = shinytheme(theme = "spacelab"), 
#   br(),
#   navbarPage(theme = shinytheme("flatly"), title= "ChangeR v.0.0.1", collapsible = TRUE,
#              tags$head(tags$style(HTML('.navbar-static-top {background-color: #006c66;}',
#                                        '.navbar-default .navbar-nav>.active>a {background-color: #006c66;}'))),
#              
#   ),
#     # Application title
#     titlePanel("An Shiny App that detects change points in Time Series"),
# 
#     # Sidebar with a slider input for number of bins 
#     sidebarLayout(
#         sidebarPanel(
#             sliderInput("bins",
#                         "Number of bins:",
#                         min = 1,
#                         max = 50,
#                         value = 30)
#         ),
# 
#         # Show a plot of the generated distribution
#         mainPanel(
#            plotOutput("distPlot")
#         )
#     )
# )
# 
# # Define server logic required to draw a histogram
# server <- function(input, output) {
# 
#     output$distPlot <- renderPlot({
#         # generate bins based on input$bins from ui.R
#         x    <- faithful[, 2]
#         bins <- seq(min(x), max(x), length.out = input$bins + 1)
# 
#         # draw the histogram with the specified number of bins
#         hist(x, breaks = bins, col = 'darkgray', border = 'white',
#              xlab = 'Waiting time to next eruption (in mins)',
#              main = 'Histogram of waiting times')
#     })
# }
# 
# # Run the application 
# shinyApp(ui = ui, server = server)
# 
# 
# 





library(shiny)
library(shinythemes)
library(mcp)
library(dplyr)

ui <- navbarPage(theme = shinytheme("flatly"), title = "ChangeR v.0.0.1", collapsible = TRUE,
                 tags$head(tags$style(HTML('.navbar-static-top {background-color: #006c66;}',
                                           '.navbar-default .navbar-nav>.active>a {background-color: #006c66;}'))),
                 
                 tabPanel("Change Point Analysis for Geoanthropology",
                          sidebarLayout(
                            sidebarPanel(
                              radioButtons("data_source", "Data Source",
                                           choices = c("Upload CSV" = "upload",
                                                       "Example Dataset" = "example"),
                                           selected = "upload"),
                              conditionalPanel(
                                condition = "input.data_source == 'upload'",
                                fileInput("file", "Choose CSV File", multiple = FALSE, accept = c(".csv"))
                              ),
                              conditionalPanel(
                                condition = "input.data_source == 'example'",
                                selectInput("example_data", "Example Dataset", choices = c("Wool" = "wool", "Melanoma" = "melanoma"))
                              ),
                              selectInput("response", "Response Variable", choices = NULL),
                              selectInput("predictors", "Predictor Variables", choices = NULL, multiple = TRUE),
                              numericInput("num_changepoints", "Number of Change Points", value = 1, min = 1),
                              actionButton("run_analysis", "Run Analysis")
                            ),
                            mainPanel(
                              tabsetPanel(
                                tabPanel("Data Preview", dataTableOutput("data_preview")),
                                tabPanel("Summary", verbatimTextOutput("summary")),
                                tabPanel("Change Point Locations", plotOutput("changepoint_plot")),
                                tabPanel("Segment Parameters", dataTableOutput("segment_params")),
                                tabPanel("Diagnostics", plotOutput("diagnostics"))
                              )
                            )
                          )
                 )
)

server <- function(input, output, session) {
  data <- reactive({
    req(input$data_source)
    if (input$data_source == "upload") {
      req(input$file)
      read.csv(input$file$datapath, header = TRUE)
    } else {
      data <- switch(input$example_data,
                     "wool" = mcp::wool,
                     "melanoma" = mcp::melanoma)
      return(data)
    }
  })
  
  observeEvent(input$file, {
    updateSelectInput(session, "response", choices = names(data()))
    updateSelectInput(session, "predictors", choices = names(data()))
  })
  
  observeEvent(input$example_data, {
    updateSelectInput(session, "response", choices = names(data()))
    updateSelectInput(session, "predictors", choices = setdiff(names(data()), "time"))
  })
  
  model <- eventReactive(input$run_analysis, {
    formula <- as.formula(paste(input$response, "~", paste(input$predictors, collapse = "+")))
    mcp_model <- mcp(formula, data = data(), nChangePoints = input$num_changepoints)
    return(mcp_model)
  })
  
  output$summary <- renderPrint({
    summary(model())
  })
  
  output$changepoint_plot <- renderPlot({
    plot(model())
  })
  
  output$segment_params <- renderDataTable({
    segment_parameters(model())
  })
  
  output$diagnostics <- renderPlot({
    bayesplot::mcmc_trace(model())
  })
}

shinyApp(ui = ui, server = server)
