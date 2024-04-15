test_that("ojo_connect creates a new connection", {
  con <- ojo_connect()
  expect_true(DBI::dbIsValid(con), "Connection should be valid")

  # Clean up: Close the connection after testing
  withr::deferred_run(envir = ojo_env())
})

test_that("ojo_connect reuses existing connection", {
  skip_on_cran()

  con1 <- ojo_connect()
  con2 <- ojo_connect()

  # Check if both connection objects point to the same connection
  expect_identical(con1, con2, "Should reuse the same connection object")

  # Clean up: Close the connection after testing
  withr::deferred_run(envir = ojo_env())
})

test_that("ojo_connect handles connection pooling correctly", {
  skip_on_cran()

  pool_con <- ojo_connect(.pool = TRUE)
  expect_true(DBI::dbIsValid(pool_con), "Pooled connection should be valid")

  # Further checks can be added to validate the pooling behavior

  # Clean up: Close the pool after testing
  withr::deferred_run(envir = ojo_env())
})
