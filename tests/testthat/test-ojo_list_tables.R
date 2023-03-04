test_that("ojo_list_tables hasn't changed", {
  expect_snapshot_value(
    ojo_list_tables("public"),
    style = "deparse"
  )
})

