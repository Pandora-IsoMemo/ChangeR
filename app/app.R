#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
# Define UI for application that draws a histogram
ui <- fluidPage(
  
  includeCSS("stylesheet.css"),
  shiny::fluidRow(
    shinydashboard::box(
      shiny::actionButton(
        inputId = 'ab1',
        label = "Need Help?",
        icon = icon("th"),
        onclick = "window.open('https://github.com/Pandora-IsoMemo/CausalR/blob/main/HELP.pdf', '_blank')",
        style="color: #fff; background-color: #006c66; border-color: #2e6da4"
      ),
      shiny::actionButton(
        inputId = 'ab2',
        label = "How to format data before upload",
        icon = icon("th"),
        onclick = "window.open('https://github.com/Pandora-IsoMemo/CausalR/blob/main/HELP.pdf', '_blank')",
        style="color: #fff; background-color: #006c66; border-color: #2e6da4"
      )
    )
  ),
  #theme = shinytheme(theme = "spacelab"), 
  br(),
  navbarPage(theme = shinytheme("flatly"), title= "ChangeR v.0.0.1", collapsible = TRUE,
             tags$head(tags$style(HTML('.navbar-static-top {background-color: #006c66;}',
                                       '.navbar-default .navbar-nav>.active>a {background-color: #006c66;}'))),
             
  ),
    # Application title
    titlePanel("An Shiny App that detects change points in Time Series"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
