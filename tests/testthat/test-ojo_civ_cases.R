test_that("ojo_civ_cases works in interactve mode", {
  rlang::with_interactive({
    testthat::expect_no_error({
      ojo_civ_cases()
    })
  })
})