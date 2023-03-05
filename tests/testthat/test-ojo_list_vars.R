test_that("ojo_list_vars hasn't changed", {
  expect_snapshot_value(
    ojo_list_vars("case"),
    style = "deparse"
  )
})

test_that("ojo_list_vars works in non-interactive mode", {
  db <- ojo_connect()

  expect_snapshot_value(
    ojo_list_vars("case", .con = db),
    style = "deparse"
  )

  pool::poolClose(db)
})
