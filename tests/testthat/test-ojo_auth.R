test_that("read_config_from_sources works correctly", {
  skip_on_runiverse()
  
  # Test with function arguments only
  func_args <- list(
    host = "test_host",
    port = "5432", 
    username = "test_user",
    password = "test_pass",
    ssl_mode = "require"
  )
  
  config <- read_config_from_sources(func_args, .admin = FALSE)
  
  expect_equal(config$host, "test_host")
  expect_equal(config$port, "5432")
  expect_equal(config$username, "test_user")
  expect_equal(config$password, "test_pass")
  expect_equal(config$ssl_mode, "require")
})

test_that("resolve_ssl_paths works with defaults", {
  skip_on_runiverse()
  
  # Mock home directory
  home_dir <- "/tmp/test_home"
  
  ssl_paths <- resolve_ssl_paths(home = home_dir)
  
  expect_equal(ssl_paths$ssl_root_cert, file.path(home_dir, ".postgresql", "ojodb", "server-ca.pem"))
  expect_equal(ssl_paths$ssl_cert, file.path(home_dir, ".postgresql", "ojodb", "client-cert.pem"))
  expect_equal(ssl_paths$ssl_key, file.path(home_dir, ".postgresql", "ojodb", "client-key.pem"))
})

test_that("resolve_ssl_paths works with custom paths", {
  skip_on_runiverse()
  
  custom_root <- "/custom/path/root.pem"
  custom_cert <- "/custom/path/cert.pem"  
  custom_key <- "/custom/path/key.pem"
  
  ssl_paths <- resolve_ssl_paths(
    ssl_root_cert = custom_root,
    ssl_cert = custom_cert,
    ssl_key = custom_key
  )
  
  expect_equal(ssl_paths$ssl_root_cert, custom_root)
  expect_equal(ssl_paths$ssl_cert, custom_cert)
  expect_equal(ssl_paths$ssl_key, custom_key)
})

test_that("validate_ssl_certs returns correct values", {
  skip_on_runiverse()
  
  # Create temporary test files
  temp_dir <- tempdir()
  test_files <- c("root.pem", "cert.pem", "key.pem")
  test_paths <- file.path(temp_dir, test_files)
  
  # Create the test files
  for (path in test_paths) {
    file.create(path)
  }
  
  ssl_paths <- list(
    ssl_root_cert = test_paths[1],
    ssl_cert = test_paths[2], 
    ssl_key = test_paths[3]
  )
  
  # Should return TRUE when all files exist
  expect_true(validate_ssl_certs(ssl_paths, strict = FALSE))
  
  # Clean up
  unlink(test_paths)
  
  # Should return FALSE when files don't exist
  expect_false(validate_ssl_certs(ssl_paths, strict = FALSE))
})

test_that("read_config_from_sources handles admin vs default users correctly", {
  skip_on_runiverse()
  
  # Set up environment variables for testing
  Sys.setenv(OJO_DEFAULT_USER = "default_user")
  Sys.setenv(OJO_DEFAULT_PASS = "default_pass")
  Sys.setenv(OJO_ADMIN_USER = "admin_user")
  Sys.setenv(OJO_ADMIN_PASS = "admin_pass")
  
  # Test default user
  func_args <- list()
  config_default <- read_config_from_sources(func_args, .admin = FALSE)
  expect_equal(config_default$username, "default_user")
  expect_equal(config_default$password, "default_pass")
  
  # Test admin user
  config_admin <- read_config_from_sources(func_args, .admin = TRUE)
  expect_equal(config_admin$username, "admin_user")
  expect_equal(config_admin$password, "admin_pass")
  
  # Clean up
  Sys.unsetenv(c("OJO_DEFAULT_USER", "OJO_DEFAULT_PASS", "OJO_ADMIN_USER", "OJO_ADMIN_PASS"))
})

test_that("ojo_auth works with admin flag", {
  skip_on_runiverse()
  
  # Create temporary SSL cert files for testing
  temp_dir <- tempdir()
  ssl_files <- c("server-ca.pem", "client-cert.pem", "client-key.pem")
  ssl_paths <- file.path(temp_dir, ssl_files)
  
  for (path in ssl_paths) {
    file.create(path)
  }
  
  # Test with .admin = TRUE and .install = FALSE
  expect_silent(
    ojo_auth(
      host = "localhost",
      port = "5432", 
      username = "adminuser",
      password = "adminpass",
      ssl_root_cert = ssl_paths[1],
      ssl_cert = ssl_paths[2],
      ssl_key = ssl_paths[3],
      .admin = TRUE,
      .install = FALSE
    )
  )
  
  # Verify admin environment variables were set
  expect_equal(Sys.getenv("OJO_HOST"), "localhost")
  expect_equal(Sys.getenv("OJO_PORT"), "5432")
  expect_equal(Sys.getenv("OJO_ADMIN_USER"), "adminuser")
  expect_equal(Sys.getenv("OJO_ADMIN_PASS"), "adminpass")
  
  # Clean up
  unlink(ssl_paths)
  Sys.unsetenv(c("OJO_HOST", "OJO_PORT", "OJO_ADMIN_USER", "OJO_ADMIN_PASS", 
                 "OJO_SSL_ROOT_CERT", "OJO_SSL_CERT", "OJO_SSL_KEY", "OJO_SSL_MODE"))
})

test_that("ojo_auth handles missing required parameters correctly", {
  skip_on_runiverse()
  
  # Should error when required parameters are missing
  expect_error(
    ojo_auth(.install = FALSE),
    "Missing required configuration parameters"
  )
})

test_that("ojo_auth works with all parameters provided", {
  skip_on_runiverse()
  
  # Create temporary SSL cert files for testing
  temp_dir <- tempdir()
  ssl_files <- c("server-ca.pem", "client-cert.pem", "client-key.pem")
  ssl_paths <- file.path(temp_dir, ssl_files)
  
  for (path in ssl_paths) {
    file.create(path)
  }
  
  # Test with .install = FALSE to avoid modifying .Renviron during tests
  expect_silent(
    ojo_auth(
      host = "localhost",
      port = "5432", 
      username = "testuser",
      password = "testpass",
      ssl_root_cert = ssl_paths[1],
      ssl_cert = ssl_paths[2],
      ssl_key = ssl_paths[3],
      .install = FALSE
    )
  )
  
  # Verify environment variables were set
  expect_equal(Sys.getenv("OJO_HOST"), "localhost")
  expect_equal(Sys.getenv("OJO_PORT"), "5432")
  expect_equal(Sys.getenv("OJO_DEFAULT_USER"), "testuser")
  expect_equal(Sys.getenv("OJO_DEFAULT_PASS"), "testpass")
  expect_equal(Sys.getenv("OJO_SSL_ROOT_CERT"), ssl_paths[1])
  
  # Clean up
  unlink(ssl_paths)
  Sys.unsetenv(c("OJO_HOST", "OJO_PORT", "OJO_DEFAULT_USER", "OJO_DEFAULT_PASS", 
                 "OJO_SSL_ROOT_CERT", "OJO_SSL_CERT", "OJO_SSL_KEY", "OJO_SSL_MODE"))
})