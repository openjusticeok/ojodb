test_that("ojo_eviction_cases works", {
  skip_if_no_db()

  testthat::expect_no_error({
    ojo_eviction_cases()
  })
})

test_that("ojo_eviction_cases includes additional case variables if specified", {
  skip_if_no_db()

  data <- ojo_eviction_cases(more_case_variables = "judge")
  expect_true("judge" %in% colnames(data))
})

test_that("ojo_eviction_cases includes additional issue variables if specified", {
  skip_if_no_db()

  data <- ojo_eviction_cases(more_issue_variables = "disposition_date")
  expect_true("disposition_date" %in% colnames(data))
})
