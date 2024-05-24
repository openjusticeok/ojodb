test_that("ojo_crim_cases works", {
  skip_on_runiverse()

  testthat::expect_no_error({
    ojo_crim_cases()
  })
})
