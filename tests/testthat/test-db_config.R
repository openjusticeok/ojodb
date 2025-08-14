test_that("db_config creates configuration objects correctly", {
  # Test basic postgres configuration
  config <- db_config("postgres")
  expect_s3_class(config, c("db_config", "list"))
  expect_equal(config$backend, "postgres")
  expect_false(config$admin)
  expect_false(config$pool)
  expect_equal(length(config$extra_args), 0)
  
  # Test admin configuration with pooling
  admin_config <- db_config("postgres", .admin = TRUE, .pool = TRUE)
  expect_equal(admin_config$backend, "postgres")
  expect_true(admin_config$admin)
  expect_true(admin_config$pool)
  
  # Test DuckDB configuration
  duck_config <- db_config("duckdb")
  expect_equal(duck_config$backend, "duckdb")
  expect_false(duck_config$admin)
  expect_false(duck_config$pool)
  
  # Test with extra arguments
  config_with_args <- db_config("postgres", custom_arg = "value", another_arg = 123)
  expect_equal(config_with_args$extra_args$custom_arg, "value")
  expect_equal(config_with_args$extra_args$another_arg, 123)
})

test_that("db_config print method works", {
  config <- db_config("postgres", .admin = TRUE, .pool = TRUE, test_arg = "test")
  
  # Capture print output
  output <- capture.output(print(config))
  
  expect_match(output[1], "Database Configuration")
  expect_match(paste(output, collapse = "\n"), "Backend: postgres")
  expect_match(paste(output, collapse = "\n"), "Admin: TRUE")
  expect_match(paste(output, collapse = "\n"), "Pool: TRUE")
  expect_match(paste(output, collapse = "\n"), "Extra args: test_arg")
})

test_that("backend configuration functions handle different scenarios", {
  # Test postgres_config with mock environment
  withr::with_envvar(
    c("OJO_HOST" = "localhost",
      "OJO_PORT" = "5432",
      "OJO_DEFAULT_USER" = "user",
      "OJO_DEFAULT_PASS" = "pass",
      "OJO_ADMIN_USER" = "admin",
      "OJO_ADMIN_PASS" = "adminpass",
      "OJO_SSL_MODE" = "verify-ca",
      "OJO_SSL_ROOT_CERT" = "/path/to/root.pem",
      "OJO_SSL_CERT" = "/path/to/cert.pem",
      "OJO_SSL_KEY" = "/path/to/key.pem"),
    {
      # Test default user config
      default_config <- postgres_config(.admin = FALSE)
      expect_equal(default_config$host, "localhost")
      expect_equal(default_config$user, "user")
      expect_equal(default_config$password, "pass")
      expect_equal(default_config$dbname, "ojodb")
      
      # Test admin user config
      admin_config <- postgres_config(.admin = TRUE)
      expect_equal(admin_config$user, "admin")
      expect_equal(admin_config$password, "adminpass")
      
      # Test with extra arguments
      config_with_args <- postgres_config(.admin = FALSE, connect_timeout = 30)
      expect_equal(config_with_args$connect_timeout, 30)
    }
  )
  
  # Test postgres_config error when no host
  withr::with_envvar(c("OJO_HOST" = ""), {
    expect_error(postgres_config(.admin = FALSE), "No default configuration")
  })
  
  # Test duckdb_config
  duck_config <- duckdb_config()
  expect_true("drv" %in% names(duck_config))
  
  # Test duckdb_config with extra args
  duck_config_args <- duckdb_config(dbdir = ":memory:")
  expect_equal(duck_config_args$dbdir, ":memory:")
})