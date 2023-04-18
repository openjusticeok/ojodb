test_that("ojo_connect works in non-interactive mode, with pool", {

  db <- ojo_connect(.pool = TRUE)

  expect_true(inherits(db, "Pool"))
  expect_true(pool::dbIsValid(db))

  pool::poolClose(db)

  expect_false(pool::dbIsValid(db))
})

# test_that("ojo_connect works in interactive mode, with pool", {
#   # rlang::with_interactive({
#
#     ojo_connect(.pool = TRUE, .global = TRUE)
#
#     db <- get("ojo_pool", envir = ojo_env(), inherits = FALSE)
#
#     expect_true(inherits(db, "Pool"))
#     expect_true(pool::dbIsValid(db))
#
#     withr::deferred_run(envir = ojo_env())
#
#     expect_false(pool::dbIsValid(db))
#
#   # })
# })

test_that("ojo_connect works in non-interactive mode, without pool", {

  db <- ojo_connect(.pool = FALSE)

  expect_true(DBI::dbIsValid(db))

})

test_that("ojo_connect works in interactive mode, without pool", {
  rlang::with_interactive({

    db <- ojo_connect(.pool = FALSE)

    expect_true(DBI::dbIsValid(db))

    withr::deferred_run(envir = ojo_env())

    expect_false(DBI::dbIsValid(db))

  })
})
