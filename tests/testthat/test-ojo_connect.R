test_that("ojo_connect works in non-interactive mode", {
  db <- ojo_connect()

  expect_true(inherits(db, "Pool"))
  expect_true(pool::dbIsValid(db))

  pool::poolClose(db)

  expect_false(pool::dbIsValid(db))
})

test_that("ojo_connect works in interactive mode", {
  rlang::with_interactive({
    ojo_connect()

    expect_true(inherits(ojo_pool, "Pool"))
    expect_true(pool::dbIsValid(ojo_pool))

    withr::deferred_run()

    expect_false(pool::dbIsValid(ojo_pool))
  })
})
