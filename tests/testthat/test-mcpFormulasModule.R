test_that("MCP Formulas Server works correctly", {
  testServer(
    mcpFormulasServer,
    {
      # Check that the apply button is initially disabled
      expect_true(is.null(session$input$apply))

      # Expect that formulasAndPriors is initially NULL
      expect_true(is.null(formulasAndPriors()))

      # Mock segment and prior matrices
      mock_segments <- matrix(c("y ~ 1 + x", "y ~ 1 + x^2"), nrow = 1, ncol = 2)
      mock_priors <- matrix(c("x_1 = dunif(-4, -0.5);", "x_2 = dnorm(0, 1);"), nrow = 1, ncol = 2)

      segmentsMatrix(mock_segments)
      priorsMatrix(mock_priors)

      # Ensure the apply button is enabled after setting valid inputs
      session$setInputs(apply = 1) # Simulate button click
      session$flushReact()

      # Define the expected output list for comparison
      expected_output <- list(
        lists_seg = list(list(as.formula("y ~ 1 + x"), as.formula("y ~ 1 + x^2"))),
        lists_prior = list(list(x_1 = "dunif(-4, -0.5)", x_2 = "dnorm(0, 1)"))
      )

      # Verify the formulas and priors reactive value is updated
      expect_true(all.equal(
        formulasAndPriors(),
        expected_output, check.attributes = FALSE)
      )
    }
  )
})
