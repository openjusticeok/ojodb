test_that("ojo_add_counts errors if data is collected", {
  df <- data.frame(id = 1, counts = 1, open_counts = 1)
  expect_error(
    ojo_add_counts(df),
    "Don't use `collect()` before this function",
    fixed = TRUE
  )
})

test_that("ojo_add_issues errors if data is collected", {
  df <- data.frame(id = 1)
  expect_error(
    ojo_add_issues(df),
    "Don't use `collect()` before this function",
    fixed = TRUE
  )
})

test_that("ojo_add_party_details errors if data is collected", {
  df <- data.frame(id = 1)
  expect_error(
    ojo_add_party_details(df),
    "Don't use `collect()` before this function",
    fixed = TRUE
  )
})

test_that("ojo_add_party_details checks for party column", {
  # Mock a lazy table
  # This is tricky without a real connection, so we'll skip for now
  # or use a very simple mock if needed.
  skip_if_no_db()

  con <- ojo_connect()
  on.exit(DBI::dbDisconnect(con))

  # Create a query that doesn't have 'party' but has 'parties'
  bad_data <- ojo_tbl("case", con = con) |>
    dplyr::mutate(parties = "test")

  expect_error(
    ojo_add_party_details(bad_data),
    "You must first unnest the `parties` column"
  )
})

test_that("ojo_add_minutes works", {
  skip_if_no_db()

  res <- ojo_tbl("case") |>
    head(1) |>
    ojo_add_minutes()

  expect_s3_class(res, "tbl_lazy")
})
