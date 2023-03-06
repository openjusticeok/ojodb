test_that("ojo_list_vars succeeds correctly in non-interactive mode", {
  db <- ojo_connect()

  expect_snapshot_value(
    ojo_list_vars("case", .con = db),
    style = "deparse"
  )

  pool::poolClose(db)

  expect_false(pool::dbIsValid(db))
})

test_that("ojo_list_vars succeeds correctly in interactive mode", {
  rlang::with_interactive({
    expect_snapshot_value(
      ojo_list_vars("case"),
      style = "deparse"
    )
  })

  expect_true(pool::dbIsValid(ojo_pool))
})