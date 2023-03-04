test_that("ojo_list_schemas hasn't changed", {
  expect_snapshot_value(
    ojo_list_schemas(),
    style = "deparse"
  )
})
