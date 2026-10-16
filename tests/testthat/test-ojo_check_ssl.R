test_that("ojo_check_ssl works with single connection", {
  skip_if_no_db()

  con <- ojo_connect()
  on.exit(DBI::dbDisconnect(con))

  # Even if it returns no rows (if not postgres), it shouldn't error
  expect_no_error(ojo_check_ssl(con))
})

test_that("ojo_check_ssl works with pool", {
  skip_if_no_db()

  pool <- ojo_pool()
  on.exit(pool::poolClose(pool))

  expect_no_error(ojo_check_ssl(pool))
})
