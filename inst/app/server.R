shinyServer(function(input, output, session) {
  #options(shiny.maxRequestSize = 400*1024^2)

  file_data <- dataServer(id = "data", path = file.path("data", "example_breakPoints.csv"))

  changePointsServer("changePoints", file_data)
})
