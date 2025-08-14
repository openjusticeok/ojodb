test_that("db_config creates basic configuration correctly", {
  skip_on_runiverse()
  
  config <- db_config(
    host = "localhost",
    port = "5432", 
    username = "testuser",
    password = "testpass"
  )
  
  expect_s3_class(config, "db_config")
  expect_equal(config$host, "localhost")
  expect_equal(config$port, "5432")
  expect_equal(config$username, "testuser")
  expect_equal(config$password, "testpass")
  expect_equal(config$ssl_mode, "verify-ca")
})

test_that("db_config reads from environment variables", {
  skip_on_runiverse()
  
  # Set up test environment variables
  Sys.setenv(OJO_HOST = "env_host")
  Sys.setenv(OJO_PORT = "env_port")
  Sys.setenv(OJO_DEFAULT_USER = "env_user")
  Sys.setenv(OJO_DEFAULT_PASS = "env_pass")
  
  config <- db_config()
  
  expect_equal(config$host, "env_host")
  expect_equal(config$port, "env_port") 
  expect_equal(config$username, "env_user")
  expect_equal(config$password, "env_pass")
  
  # Clean up
  Sys.unsetenv(c("OJO_HOST", "OJO_PORT", "OJO_DEFAULT_USER", "OJO_DEFAULT_PASS"))
})

test_that("db_config handles admin credentials correctly", {
  skip_on_runiverse()
  
  # Set up test environment variables
  Sys.setenv(OJO_ADMIN_USER = "admin_user")
  Sys.setenv(OJO_ADMIN_PASS = "admin_pass")
  Sys.setenv(OJO_DEFAULT_USER = "default_user")
  Sys.setenv(OJO_DEFAULT_PASS = "default_pass")
  
  # Test admin mode
  admin_config <- db_config(.admin = TRUE)
  expect_equal(admin_config$username, "admin_user")
  expect_equal(admin_config$password, "admin_pass")
  
  # Test default mode
  default_config <- db_config(.admin = FALSE)
  expect_equal(default_config$username, "default_user") 
  expect_equal(default_config$password, "default_pass")
  
  # Clean up
  Sys.unsetenv(c("OJO_ADMIN_USER", "OJO_ADMIN_PASS", "OJO_DEFAULT_USER", "OJO_DEFAULT_PASS"))
})

test_that("db_config function arguments override environment", {
  skip_on_runiverse()
  
  # Set up environment variables
  Sys.setenv(OJO_HOST = "env_host")
  Sys.setenv(OJO_PORT = "env_port")
  
  # Function arguments should override
  config <- db_config(
    host = "arg_host",
    port = "arg_port", 
    username = "arg_user",
    password = "arg_pass"
  )
  
  expect_equal(config$host, "arg_host")
  expect_equal(config$port, "arg_port")
  expect_equal(config$username, "arg_user")
  expect_equal(config$password, "arg_pass")
  
  # Clean up
  Sys.unsetenv(c("OJO_HOST", "OJO_PORT"))
})

test_that("db_config sets default SSL paths", {
  skip_on_runiverse()
  
  config <- db_config(host = "test", port = "5432", username = "user", password = "pass")
  
  home <- Sys.getenv("HOME")
  expected_dir <- file.path(home, ".postgresql", "ojodb")
  
  expect_equal(config$ssl_root_cert, file.path(expected_dir, "server-ca.pem"))
  expect_equal(config$ssl_cert, file.path(expected_dir, "client-cert.pem"))
  expect_equal(config$ssl_key, file.path(expected_dir, "client-key.pem"))
})

test_that("ojo_auth validates db_config input", {
  skip_on_runiverse()
  
  # Should error with non-db_config object
  expect_error(ojo_auth(list(host = "test")), "db_config must be a db_config object")
  
  # Should error with missing required fields
  incomplete_config <- structure(list(host = "test"), class = c("db_config", "list"))
  expect_error(ojo_auth(incomplete_config), "Missing required configuration")
})

test_that("ojo_auth works with session-only mode", {
  skip_on_runiverse()
  
  config <- db_config(
    host = "localhost",
    port = "5432",
    username = "testuser", 
    password = "testpass"
  )
  
  # Test with .install = FALSE
  expect_silent(ojo_auth(config, .install = FALSE))
  
  # Verify environment variables were set
  expect_equal(Sys.getenv("OJO_HOST"), "localhost")
  expect_equal(Sys.getenv("OJO_PORT"), "5432")
  expect_equal(Sys.getenv("OJO_DEFAULT_USER"), "testuser")
  expect_equal(Sys.getenv("OJO_DEFAULT_PASS"), "testpass")
  
  # Clean up
  Sys.unsetenv(c("OJO_HOST", "OJO_PORT", "OJO_DEFAULT_USER", "OJO_DEFAULT_PASS", 
                 "OJO_SSL_ROOT_CERT", "OJO_SSL_CERT", "OJO_SSL_KEY", "OJO_SSL_MODE"))
})

test_that("ojo_auth detects admin users correctly", {
  skip_on_runiverse()
  
  admin_config <- db_config(
    host = "localhost",
    port = "5432", 
    username = "adminuser",
    password = "adminpass"
  )
  
  expect_silent(ojo_auth(admin_config, .install = FALSE))
  
  # Should set admin environment variables
  expect_equal(Sys.getenv("OJO_ADMIN_USER"), "adminuser")
  expect_equal(Sys.getenv("OJO_ADMIN_PASS"), "adminpass")
  expect_equal(Sys.getenv("OJO_DEFAULT_USER"), "")
  expect_equal(Sys.getenv("OJO_DEFAULT_PASS"), "")
  
  # Clean up
  Sys.unsetenv(c("OJO_HOST", "OJO_PORT", "OJO_ADMIN_USER", "OJO_ADMIN_PASS", 
                 "OJO_SSL_ROOT_CERT", "OJO_SSL_CERT", "OJO_SSL_KEY", "OJO_SSL_MODE"))
})

test_that("db_config reads from YAML file", {
  skip_on_runiverse()
  skip_if_not_installed("yaml")
  
  # Create temporary YAML file
  temp_yaml <- tempfile(fileext = ".yaml")
  yaml_content <- "
host: yaml_host
port: yaml_port
username: yaml_user
password: yaml_pass
ssl_mode: require
"
  writeLines(yaml_content, temp_yaml)
  
  config <- db_config(config_file = temp_yaml)
  
  expect_equal(config$host, "yaml_host")
  expect_equal(config$port, "yaml_port")
  expect_equal(config$username, "yaml_user")
  expect_equal(config$password, "yaml_pass")
  expect_equal(config$ssl_mode, "require")
  
  # Clean up
  unlink(temp_yaml)
})

test_that("db_config handles nested YAML format", {
  skip_on_runiverse()
  skip_if_not_installed("yaml")
  
  # Create temporary YAML file with nested structure
  temp_yaml <- tempfile(fileext = ".yaml")
  yaml_content <- "
ojo:
  host: nested_host
  port: nested_port
  username: nested_user
  password: nested_pass
"
  writeLines(yaml_content, temp_yaml)
  
  config <- db_config(config_file = temp_yaml)
  
  expect_equal(config$host, "nested_host")
  expect_equal(config$port, "nested_port")
  expect_equal(config$username, "nested_user")
  expect_equal(config$password, "nested_pass")
  
  # Clean up
  unlink(temp_yaml)
})