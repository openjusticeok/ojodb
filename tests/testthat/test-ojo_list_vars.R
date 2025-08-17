test_that("ojo_list_vars hasn't changed", {
  skip_if_no_db()

  expect_snapshot_value(
    ojo_list_vars("case"),
    style = "deparse"
  )
})
