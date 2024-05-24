test_that("ojo_crim_cases works", {
  testthat::skip_on_cran()

  testthat::expect_no_error({
    ojo_crim_cases()
  })
})
