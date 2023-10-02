test_that("ojo_tbl succeeds correctly in non-interactive mode", {
  db <- ojo_connect(.pool = TRUE)

  expect_snapshot_value(
    ojo_tbl("case", .con = db),
    style = "serialize"
  )
  expect_true(pool::dbIsValid(db))

  pool::poolClose(db)
  expect_false(pool::dbIsValid(db))

  expect_no_error(ojo_tbl("case", .global = TRUE))
  withr::deferred_run(envir = ojo_env())
})

# test_that("ojo_tbl fails correctly in non-interactive mode", {
#   expect_error({
#     ojo_tbl("case")
#   })
#   expect_error({
#     ojo_tbl("case")
#     pool::dbIsValid(ojo_pool)
#   })
# })

test_that("ojo_tbl succeeds correctly in interactive mode", {
  rlang::with_interactive({
    ojo_connect(.pool = TRUE)

    db <- get("ojo_pool", envir = ojo_env(), inherits = FALSE)

    expect_true(pool::dbIsValid(db))

    expect_snapshot_value(
      ojo_tbl("case"),
      style = "serialize"
    )

    expect_true(pool::dbIsValid(db))

    withr::deferred_run(envir = ojo_env())

    expect_false(exists("ojo_pool", envir = ojo_env(), inherits = FALSE))
  })
})

# test_that("ojo_tbl fails correctly in interactive mode", {
#   rlang::with_interactive({
#     expect_false(exists("ojo_pool", envir = ojo_env(), inherits = FALSE))
#     expect_error(ojo_tbl("case", .global = FALSE))
#     expect_false(exists("ojo_pool", envir = ojo_env(), inherits = FALSE))
#   })
# })
