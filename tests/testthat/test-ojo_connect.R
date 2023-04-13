test_that("ojo_connect works in non-interactive mode", {
  db <- ojo_connect(.pool = TRUE)

  expect_true(inherits(db, "Pool"))
  expect_true(pool::dbIsValid(db))

  pool::poolClose(db)

  expect_false(pool::dbIsValid(db))
})

test_that("ojo_connect works in interactive mode", {
  rlang::with_interactive({
    ojo_connect(.pool = TRUE)

    db <- get("ojo_pool", envir = ojo_env(), inherits = FALSE)
    expect_true(inherits(db, "Pool"))
    expect_true(pool::dbIsValid(db))

    withr::deferred_run(envir = ojo_env())

    expect_false(pool::dbIsValid(db))
  })
})
