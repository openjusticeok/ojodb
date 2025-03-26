test_that("ojo_query executes SQL and returns a tibble", {
  skip_on_runiverse()

  # A simple, safe query that assumes the existence of a 'case' table
  query <- 'SELECT * FROM "case" LIMIT 10'

  result <- ojo_query(query)

  # Check that it returns a lazy tibble
  expect_s3_class(result, "tbl_lazy")

  # Check the result is not empty and has expected number of rows
  expect_equal(
    result |>
      collect() |>
      nrow(),
    10L
  )

  withr::deferred_run(envir = ojo_env())
})

# Test error handling for malformed queries
test_that("ojo_query handles SQL errors", {
  skip_on_runiverse()

  # Intentionally malformed SQL query
  bad_query <- "SELEC * FROM nonexistent_table"

  expect_error(
    ojo_query(bad_query),
    "Can't query fields",
    fixed = TRUE
  )

  withr::deferred_run(envir = ojo_env())
})
