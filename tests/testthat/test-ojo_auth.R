test_that("db_config creates basic configuration correctly", {
  config <- db_config(
    host = "localhost",
    port = "5432",
    username = "testuser",
    password = "testpass",
    ssl_mode = "require"
  )

  expect_s3_class(config, "db_config")
  expect_equal(config$host, "localhost")
  expect_equal(config$port, "5432")
  expect_equal(config$username, "testuser")
  expect_equal(config$password, "testpass")
  expect_equal(config$ssl_mode, "require")
})

test_that("db_config reads from environment variables", {
  # withr::with_envvar() sets environment variables only for the code
  # inside the block, then automatically restores the previous state.
  withr::with_envvar(
    new = c(
      OJO_HOST = "env_host",
      OJO_PORT = "env_port",
      OJO_USER = "env_user",
      OJO_PASS = "env_pass",
      OJO_SSL_MODE = "env_ssl_mode"
    ),
    {
      config <- db_config()

      expect_equal(config$host, "env_host")
      expect_equal(config$port, "env_port")
      expect_equal(config$username, "env_user")
      expect_equal(config$password, "env_pass")
      expect_equal(config$ssl_mode, "env_ssl_mode")
    }
  )
})

test_that("db_config function arguments override environment", {
  withr::with_envvar(
    new = c(OJO_HOST = "env_host", OJO_PORT = "env_port"),
    {
      # Function arguments should override the environment variables
      config <- db_config(
        host = "arg_host",
        port = "arg_port",
        username = "arg_user",
        password = "arg_pass",
        ssl_mode = "arg_ssl_mode"
      )

      expect_equal(config$host, "arg_host")
      expect_equal(config$port, "arg_port")
      expect_equal(config$username, "arg_user")
      expect_equal(config$password, "arg_pass")
      expect_equal(config$ssl_mode, "arg_ssl_mode")
    }
  )
})

test_that("ojo_auth validates db_config input", {
  expect_error(
    ojo_auth(db_config = list(host = "test")),
    "db_config must be a db_config object"
  )

  incomplete_config <- structure(
    list(host = "test"),
    class = c("db_config", "list")
  )

  expect_error(
    ojo_auth(db_config = incomplete_config),
    "Missing required configuration"
  )
})

test_that("ojo_auth works with session-only mode", {
  # Capture current state of OJO variables to restore them later
  ojo_vars <- c("OJO_DRIVER", "OJO_DATABASE", "OJO_HOST", "OJO_PORT", "OJO_USER", "OJO_PASS", "OJO_SSL_MODE", "OJO_SSL_ROOT_CERT", "OJO_SSL_CERT", "OJO_SSL_KEY")
  old_vars <- Sys.getenv(ojo_vars, names = TRUE)
  
  # Restore them when the test finishes
  withr::defer({
    do.call(Sys.setenv, as.list(old_vars))
  })

  config <- db_config(
    driver = "RPostgres",
    database = "ojodb",
    host = "localhost",
    port = "5432",
    username = "testuser",
    password = "testpass",
    ssl_mode = "require"
  )

  capture.output({
    ojo_auth(db_config = config, .install = FALSE)
  })

  # Verify environment variables were set correctly
  expect_equal(Sys.getenv("OJO_HOST"), "localhost")
  expect_equal(Sys.getenv("OJO_PORT"), "5432")
  expect_equal(Sys.getenv("OJO_USER"), "testuser")
  expect_equal(Sys.getenv("OJO_PASS"), "testpass")
  expect_equal(Sys.getenv("OJO_SSL_MODE"), "require")
})

test_that("db_config reads from YAML file", {
  temp_yaml <- withr::local_tempfile(fileext = ".yaml")

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
})

test_that("db_config handles nested YAML format", {
  temp_yaml <- withr::local_tempfile(fileext = ".yaml")

  yaml_content <- "
ojodb:
  host: nested_host
  port: nested_port
  username: nested_user
  password: nested_pass
  ssl_mode: nested_ssl_mode
"
  writeLines(yaml_content, temp_yaml)

  config <- db_config(config_file = temp_yaml)

  expect_equal(config$host, "nested_host")
  expect_equal(config$port, "nested_port")
  expect_equal(config$username, "nested_user")
  expect_equal(config$password, "nested_pass")
  expect_equal(config$ssl_mode, "nested_ssl_mode")
})
