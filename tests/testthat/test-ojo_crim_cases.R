test_that("ojo_crim_cases works in interactve mode", {
  rlang::with_interactive({
    testthat::expect_no_error({
      ojo_crim_cases()
    })
  })
})