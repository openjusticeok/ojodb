test_that("ojo_list_vars returns expected columns", {
  skip_if_no_db()

  vars <- ojo_list_vars("case")
  
  expect_s3_class(vars, "data.frame")
  expect_true("column_name" %in% names(vars))
  
  # Check for some standard columns that should always exist
  expected_vars <- c("case_number", "date_filed", "district", "id")
  expect_true(all(expected_vars %in% vars$column_name))
})
