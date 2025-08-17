# Helper to ensure a clean slate for each test
# This is crucial for testing the singleton pattern reliably.
with_clean_ojo_env <- function(code) {
  # Store the current state of the environment
  old_env <- as.list(.ojo_env)
  # Clear the environment for the test
  rm(list = ls(envir = .ojo_env), envir = .ojo_env)

  # Ensure the environment is restored even if the test fails
  on.exit({
    rm(list = ls(envir = .ojo_env), envir = .ojo_env)
    rlang::env_bind(.ojo_env, !!!old_env)
  })

  # Run the test code in the cleaned environment
  force(code)
}

test_that("ojo_connect creates a side-effect-free connection", {
  skip_if_no_db()

  con <- ojo_connect()
  # Defer disconnection to ensure it happens even if tests fail
  withr::defer(DBI::dbDisconnect(con))

  expect_true(DBI::dbIsValid(con), "Connection should be valid")

  # The key test: the package environment should be empty because
  # ojo_connect() does not have side effects.
  expect_equal(length(ls(envir = .ojo_env)), 0)
})

test_that("ojo_default_connection creates and caches a connection", {
  skip_if_no_db()

  with_clean_ojo_env({
    # 1. First call: creates a new connection
    con1 <- ojo_default_connection()
    expect_true(DBI::dbIsValid(con1))

    # 2. Second call: should return the exact same object
    con2 <- ojo_default_connection()
    expect_identical(con1, con2, "Should reuse the cached singleton connection")

    # 3. Manually created connection should be different
    manual_con <- ojo_connect()
    withr::defer(DBI::dbDisconnect(manual_con)) # Clean up manual connection
    expect_false(
      identical(con1, manual_con),
      "Default and manual connections should be different objects"
    )

    # Clean up the default connection at the end of the test
    withr::deferred_run()
  })
})

test_that("ojo_default_connection handles different backends separately", {
  skip_if_no_db()

  with_clean_ojo_env({
    # Create and cache a default Postgres connection
    pg_con <- ojo_default_connection()
    expect_s4_class(pg_con, "PqConnection")

    # Create and cache a default DuckDB connection
    duckdb_config <- db_config(driver = "duckdb")
    duck_con <- ojo_default_connection(db_config = duckdb_config)
    expect_s4_class(duck_con, "duckdb_connection")

    # Ensure they are not the same object
    expect_false(identical(pg_con, duck_con))

    # Verify that calling again returns the correct cached versions
    expect_identical(pg_con, ojo_default_connection())
    expect_identical(
      duck_con,
      ojo_default_connection(db_config = duckdb_config)
    )
  })
})
