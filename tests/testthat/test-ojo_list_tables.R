test_that("ojo_list_tables returns expected tables", {
  skip_if_no_db()

  tables <- ojo_list_tables("public")
  
  expect_s3_class(tables, "data.frame")
  expect_true(all(c("schema", "table") %in% names(tables)))
  expect_true(all(tables$schema == "public"))
  
  # Check for some standard tables that should always exist
  expected_tables <- c("case", "party", "minute", "attorney")
  expect_true(all(expected_tables %in% tables$table))
})
