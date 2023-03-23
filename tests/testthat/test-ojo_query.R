test_that("ojo_query succeeds correctly in non-interactive mode", {
  db <- ojo_connect()
  expect_snapshot(ojo_query("SELECT 'a' as test;", .con = db))
  expect_true(pool::dbIsValid(db))

  pool::poolClose(db)
  expect_false(pool::dbIsValid(db))
})

# test_that("ojo_query fails correctly in non-interactive mode", {
#   expect_error(
#     ojo_query("SELECT 'a' as test;")
#   )
# })

test_that("ojo_query succeeds with global flag in non-interactive mode", {
  expect_snapshot(ojo_query("SELECT 'a' as test;", .global = TRUE))
})

test_that("ojo_query succeeds correctly in interactive mode", {
  rlang::with_interactive({
    expect_snapshot(ojo_query("SELECT 'a' as test;"))
  })
})

# test_that("ojo_query fails correctly in interactive mode", {
#   rlang::with_interactive({
#     expect_error(
#       ojo_query("SELECT 'a' as test;", .global = FALSE)
#     )
#   })
# })
