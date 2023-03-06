test_that("ojo_tbl succeeds correctly in non-interactive mode", {
  db <- ojo_connect()
  expect_snapshot_value(
    ojo_tbl("case", .con = db),
    style = "serialize"
  )
  expect_true(pool::dbIsValid(ojo_pool))

  pool::poolClose(db)
  expect_false(pool::dbIsValid(ojo_pool))

  expect_no_error(ojo_tbl("case", .global = TRUE))
})

test_that("ojo_tbl fails correctly in non-interactive mode", {
  expect_error(ojo_tbl("case"))
  expect_error(pool::dbIsValid(ojo_pool))
})

test_that("ojo_tbl succeeds correctly in interactive mode", {
  rlang::with_interactive({
    ojo_connect()

    expect_true(pool::dbIsValid(ojo_pool))

    expect_snapshot_value(
      ojo_tbl("case"),
      style = "serialize"
    )

    expect_true(pool::dbIsValid(ojo_pool))

    withr::deferred_run()

    expect_false(pool::dbIsValid(ojo_pool))
  })
})

test_that("ojo_tbl fails correctly in interactive mode", {
  rlang::with_interactive({
    expect_error(pool::dbIsValid(ojo_pool))
    expect_error(ojo_tbl("case", .global = FALSE))
    expect_error(pool::dbIsValid(ojo_pool))
  })
})