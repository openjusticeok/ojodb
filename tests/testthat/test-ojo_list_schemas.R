test_that("ojo_list_schemas hasn't changed", {
  skip_if_no_db()

  expect_snapshot_value(
    ojo_list_schemas(),
    style = "deparse"
  )
})
