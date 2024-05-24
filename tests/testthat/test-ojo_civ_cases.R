test_that("ojo_civ_cases works", {
  skip_on_runiverse()

  testthat::expect_no_error({
    ojo_civ_cases()
  })
})
