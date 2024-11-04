testthat::test_that("detectBreakPoints", {
  # load test data
  df <- testthat::test_path("testdata/test_detectBreakPoints.csv") %>%
    read.csv()

  segmentsMatrix <- matrix(
    c(
      "d15N ~ 1 + time",
      "d15N ~ 1 ~ 0 + time",
      "d15N ~ 1 ~ 0 + time",
      "d15N ~ 1 ~ 0 + time",
      "d15N ~ 1 + time",
      "",
      "",
      "",
      "",
      "",
      "",
      ""
    ),
    nrow = 3,
    ncol = 4,
    byrow = TRUE
  )

  priorsMatrix <- matrix(
    c(
      "time_1 = dunif(-4, -0.5);",
      "",
      "",
      "",
      "",
      "",
      "",
      "",
      "",
      "",
      "",
      ""
    ),
    nrow = 3,
    ncol = 4,
    byrow = TRUE
  )

  comb <- getComb(segments = segmentsMatrix, priors = priorsMatrix)

  testthat::expect_equal(comb[1, 1], "d15N ~ 1 + time*+*time_1 = dunif(-4, -0.5);")
  testthat::expect_equal(dim(comb), c(16, 4))
  testthat::expect_equal(comb %>% cleanComb() %>% dim(), c(8, 4))

  twoMatrices <- comb %>% cleanComb() %>% splitComb()

  testthat::expect_equal(twoMatrices$mat1[1, 1], "d15N ~ 1 + time")
  testthat::expect_equal(twoMatrices$mat2[1, 1], "time_1 = dunif(-4, -0.5);")
  testthat::expect_equal(twoMatrices$mat1 %>% dim(), c(8, 4))
  testthat::expect_equal(twoMatrices$mat2 %>% dim(), c(8, 4))

  lists <- comb %>%
    cleanComb() %>%
    splitComb() %>%
    setFormulasAndPriors()

  res <- runMcp(lists = lists, data = df)

  testthat::expect_equal(
    compareWithLoo(res) %>% colnames() %>% suppressWarnings(),
    c(
      "elpd_diff",
      "se_diff",
      "elpd_loo",
      "se_elpd_loo",
      "p_loo",
      "se_p_loo",
      "looic",
      "se_looic"
    )
  )
})


