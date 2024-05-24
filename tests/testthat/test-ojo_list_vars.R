test_that("ojo_list_vars hasn't changed", {
  testthat::skip_on_cran()

  expect_snapshot_value(
    ojo_list_vars("case"),
    style = "deparse"
  )

  withr::deferred_run(envir = ojo_env())
})
