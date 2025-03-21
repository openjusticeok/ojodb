test_that("ojo_list_schemas hasn't changed", {
  skip_on_ci()
  skip_on_runiverse()

  expect_snapshot_value(
    ojo_list_schemas(),
    style = "deparse"
  )
})
