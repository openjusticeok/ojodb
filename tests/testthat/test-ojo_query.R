test_that("ojo_query executes SQL and returns a tibble", {
  # A simple, safe query that assumes the existence of a 'case' table
  query <- 'SELECT * FROM "case" LIMIT 10'

  result <- ojo_query(query)

  # Check that it returns a tibble
  expect_s3_class(result, "tbl")

  # Check the result is not empty and has expected number of rows
  expect_equal(nrow(result), 10)

  withr::deferred_run(envir = ojo_env())
})

# Test error handling for malformed queries
test_that("ojo_query handles SQL errors", {
  # Intentionally malformed SQL query
  bad_query <- "SELEC * FROM nonexistent_table"

  expect_error(
    ojo_query(bad_query),
    "syntax error at or near \"SELEC\"",
    fixed = TRUE
  )

  withr::deferred_run(envir = ojo_env())
})
