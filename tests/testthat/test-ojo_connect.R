test_that("ojo_connect creates a new connection", {
  skip_on_runiverse()

  con <- ojo_connect()
  expect_true(DBI::dbIsValid(con), "Connection should be valid")

  # Clean up: Close the connection after testing
  withr::deferred_run(envir = ojo_env())
})

test_that("ojo_connect reuses existing connection", {
  skip_on_runiverse()

  con1 <- ojo_connect()
  con2 <- ojo_connect()

  # Check if both connection objects point to the same connection
  expect_identical(con1, con2, "Should reuse the same connection object")

  # Clean up: Close the connection after testing
  withr::deferred_run(envir = ojo_env())
})

test_that("ojo_connect handles connection pooling correctly", {
  skip_on_runiverse()

  pool_con <- ojo_connect(.pool = TRUE)
  expect_true(DBI::dbIsValid(pool_con), "Pooled connection should be valid")

  # Further checks can be added to validate the pooling behavior

  # Clean up: Close the pool after testing
  withr::deferred_run(envir = ojo_env())
})

test_that("ojo_connect supports local connections without side effects", {
  skip_on_runiverse()

  # Create a local connection
  local_con <- ojo_connect(.local = TRUE)
  expect_true(DBI::dbIsValid(local_con), "Local connection should be valid")
  
  # Check that no connection is stored in the environment
  expect_false(exists("ojo_con_RPostgres", envir = ojo_env()), 
               "Local connection should not be stored in environment")
  
  # Clean up manually (since it's local)
  DBI::dbDisconnect(local_con)
})

test_that("db_config creates proper configuration objects", {
  # Test basic configuration
  config <- db_config("postgres")
  expect_s3_class(config, "db_config")
  expect_equal(config$backend, "postgres")
  expect_false(config$admin)
  expect_false(config$pool)
  
  # Test admin configuration with pooling
  admin_config <- db_config("postgres", .admin = TRUE, .pool = TRUE)
  expect_true(admin_config$admin)
  expect_true(admin_config$pool)
  
  # Test DuckDB configuration
  duck_config <- db_config("duckdb")
  expect_equal(duck_config$backend, "duckdb")
})

test_that("ojo_connect works with db_config objects", {
  skip_on_runiverse()
  
  # Test with configuration object
  config <- db_config("postgres", .admin = FALSE)
  con <- ojo_connect(config, .local = TRUE)
  expect_true(DBI::dbIsValid(con), "Connection from config should be valid")
  
  # Clean up
  DBI::dbDisconnect(con)
})

test_that("backend configuration functions work correctly", {
  # Mock environment variables for testing
  withr::with_envvar(
    c("OJO_HOST" = "test.host",
      "OJO_PORT" = "5432",
      "OJO_DEFAULT_USER" = "testuser",
      "OJO_DEFAULT_PASS" = "testpass",
      "OJO_SSL_MODE" = "verify-ca"),
    {
      config <- postgres_config(.admin = FALSE)
      expect_equal(config$host, "test.host")
      expect_equal(config$port, "5432")
      expect_equal(config$user, "testuser")
      expect_equal(config$dbname, "ojodb")
    }
  )
  
  # Test DuckDB config
  duck_config <- duckdb_config()
  expect_true("drv" %in% names(duck_config))
})
