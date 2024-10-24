test_that("mcpDataServer updates and processes data selections correctly", {
  # Define a mock reactive file data input
  file_data <- reactiveVal(data.frame(
    time = c(1, 2, 3, 4, 5),
    value = c(10, 20, 30, 40, 50),
    category = c("A", "B", "A", "B", "A")
  ))

  # Run the server test
  testServer(
    mcpDataServer,
    args = list(file_data = file_data),
    {
      # Verify that x and y choices are updated correctly
      session$setInputs(x = "time")
      session$setInputs(y = "value")
      session$flushReact()

      expect_equal(session$returned()$x, as.numeric(file_data()$time))
      expect_equal(session$returned()$y, file_data()$value)
    }
  )
})

test_that("mcpModelingServer enables Run MCP button and returns model output", {
  # Define mock formulas and priors
  mock_formulas <- reactiveVal(list(
    lists_seg = list(list(y ~ 1 + x)),
    lists_prior = list(list(x_1 = "dunif(-4, -0.5)"))
  ))

  # Define mock data
  mock_data <- reactiveVal(data.frame(
    x = 1:10,
    y = rnorm(10)
  ))

  # Run the server test
  testServer(
    mcpModelingServer,
    args = list(formulasAndPriors = mock_formulas, mcpData = mock_data),
    {
      # Set inputs for MCP model parameters
      session$setInputs(adapt = 5000)
      session$setInputs(chains = 3)
      session$setInputs(iter = 3000)

      # Simulate clicking the "Run MCP" button
      session$setInputs(apply = 1)
      session$flushReact()

      # Check that the module's output contains the expected mock result
      expect_true(length(session$returned()) > 0)
    }
  )
})
