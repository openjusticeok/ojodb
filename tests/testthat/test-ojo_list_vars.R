test_that("ojo_list_vars hasn't changed", {
  skip_on_runiverse()

  expect_snapshot_value(
    ojo_list_vars("case"),
    style = "deparse"
  )

  withr::deferred_run(envir = ojo_env())
})
