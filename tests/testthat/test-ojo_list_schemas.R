test_that("ojo_list_schemas hasn't changed", {
  testthat::skip_on_cran()

  expect_snapshot_value(
    ojo_list_schemas(),
    style = "deparse"
  )
})
