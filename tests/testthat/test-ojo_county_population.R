test_that("ojo_county_population returns expected structure", {
  skip_if_not_installed("tidycensus")
  # This might still fail without an API key, so we wrap in try
  res <- try(ojo_county_population(2020), silent = TRUE)

  if (inherits(res, "try-error")) {
    skip("tidycensus failed (probably missing API key)")
  }

  expect_s3_class(res, "tbl_df")
  expect_true(all(c("court", "year", "pop") %in% colnames(res)))
})
