test_that("matrixServer initializes and handles cell updates correctly", {
  # Define a mock example function to return a preset matrix
  example_function <- function() {
    matrix(data = c("1 + 1", "2 + 2", "3 + 3", "4 + 4"), nrow = 2, ncol = 2)
  }

  # Define a mock validation function that returns the input as is
  validate_function <- function(x) {
    return(x)
  }

  # Run the server test
  testServer(
    matrixServer,
    args = list(exampleFunction = example_function, validateCellFunction = validate_function),
    {
      # Set the matrix dimensions to 2x2 and check that the matrix updates accordingly
      session$setInputs(rows = 2, cols = 2, new = 1)
      session$flushReact()

      expect_equal(nrow(session$returned()), 2)
      expect_equal(ncol(session$returned()), 2)

      # Set a cell value and verify it is updated in the matrix
      session$setInputs(cellID = 1, cell = "5 + 5", set = 1)
      session$flushReact()

      expect_equal(session$returned()[1, 1], "5 + 5")

      # Load example data and verify the matrix contents
      session$setInputs(example = 1)
      session$flushReact()

      expect_equal(session$returned(), example_function())
      expect_equal(nrow(session$returned()), 2)
      expect_equal(ncol(session$returned()), 2)
    }
  )
})

testthat::test_that("getCellChoices", {
  testthat::expect_equal(getCellChoices(nrow = 3, ncol = 2), structure(
    1:6,
    names = c("(1, 1)", "(2, 1)", "(3, 1)", "(1, 2)", "(2, 2)", "(3, 2)")
  ))
})
