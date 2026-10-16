test_that("ojo_list_schemas returns expected schemas", {
  skip_if_no_db()

  schemas <- ojo_list_schemas()
  
  expect_s3_class(schemas, "data.frame")
  expect_true("schema" %in% names(schemas))
  expect_true("public" %in% schemas$schema)
})
