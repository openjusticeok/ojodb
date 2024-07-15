test_that("ojo_eviction_cases works", {
  skip_on_runiverse()

  testthat::expect_no_error({
    ojo_eviction_cases()
  })
})
