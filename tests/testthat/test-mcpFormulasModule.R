test_that("MCP Formulas Server works correctly", {
  testServer(
    mcpFormulasServer,
    {
      # Check that the apply button is initially disabled
      expect_true(is.null(session$input$apply))

      # Expect that formulasAndPriors is initially NULL
      expect_true(is.null(session$returned$formulasAndPriors()))

      # Mock segment and prior matrices
      mock_segments <- matrix(c("y ~ 1 + x", "y ~ 1 + x^2"), nrow = 1, ncol = 2)
      mock_priors <- matrix(c("x_1 = dunif(-4, -0.5);", "x_2 = dnorm(0, 1);"), nrow = 1, ncol = 2)

      formulasList$segmentsMatrix(mock_segments)
      formulasList$priorsMatrix(mock_priors)

      # Ensure the apply button is enabled after setting valid inputs
      session$flushReact()

      # Define the expected output list for comparison
      expected_output <- list(lists_seg = list(list(y ~ 1 + x, y ~ 1 + x^2)), lists_prior = list(
        list(x_1 = "dunif(-4, -0.5)", x_2 = "dnorm(0, 1)")))

      # Verify the formulas and priors reactive value is updated
      expect_true(all.equal(
        session$returned$formulasAndPriors(),
        expected_output
      ))
    }
  )
})

testthat::test_that("validateFormula", {
  testthat::expect_equal(validateFormula("y ~ x + 1"), "y ~ x + 1")
  testthat::expect_equal(validateFormula("~ 1 + x"), "~ 1 + x")
  testthat::expect_equal(validateFormula("~ I(x^2) + exp(x) + sin(x)"),
                         "~ I(x^2) + exp(x) + sin(x)")
  testthat::expect_equal(validateFormula("~sigma(1)"), "~sigma(1)")
  testthat::expect_equal(validateFormula("~sigma(rel(1) + I(x^2))"),
                         "~sigma(rel(1) + I(x^2))")
  testthat::expect_equal(validateFormula("~ar(1)"), "~ar(1)")
  testthat::expect_warning(validateFormula("y <- x + 1"))
})
