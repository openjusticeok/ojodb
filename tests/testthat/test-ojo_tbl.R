test_that("ojo_tbl retrieves a table correctly", {
  tbl <- ojo_tbl("case")

  # Check that it returns a tibble
  expect_s3_class(tbl, "tbl")

  withr::deferred_run(envir = ojo_env())
})

# Test error handling for non-existent table
test_that("ojo_tbl handles non-existent tables correctly", {
  non_existent_table <- "should_not_exist"
  expect_error(
    ojo_tbl(non_existent_table),
    'relation "public.should_not_exist" does not exist',
    fixed = TRUE
  )

  withr::deferred_run(envir = ojo_env())
})

test_that("ojo_tbl's can join", {
  skip_on_runiverse()

  tbl <- ojo_tbl("case")
  tbl2 <- ojo_tbl("minute")

  # Check that it returns a tibble
  expect_s3_class(tbl, "tbl")

  # Check that it returns a tibble
  expect_s3_class(tbl2, "tbl")

  # Check that it can join
  expect_s3_class(tbl |>
    dplyr::left_join(tbl2, by = c("id" = "case_id")),
    "tbl")

  withr::deferred_run(envir = ojo_env())
})
