test_that("ojo_case_types works", {
  skip_if_no_db()

  res <- ojo_case_types()
  expect_s3_class(res, "tbl_df")
  expect_true("case_type" %in% colnames(res))
})
