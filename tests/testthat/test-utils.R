test_that("ojo_version returns the package version", {
  expect_equal(ojo_version(), utils::packageVersion("ojodb"))
})

test_that("ojo_fiscal_year calculates correctly", {
  # June 30, 2018 should be FY 2018
  expect_equal(ojo_fiscal_year(as.Date("2018-06-30")), 2018)

  # July 1, 2018 should be FY 2019
  expect_equal(ojo_fiscal_year(as.Date("2018-07-01")), 2019)

  # Vectorized input
  dates <- as.Date(c("2018-06-30", "2018-07-01"))
  expect_equal(ojo_fiscal_year(dates), c(2018, 2019))
})
