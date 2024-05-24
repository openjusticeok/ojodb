test_that("ojo_civ_cases works", {
  testthat::skip_on_cran()

  testthat::expect_no_error({
    ojo_civ_cases()
  })
})
