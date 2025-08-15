skip_if_no_db()

test_that("ojo_crim_cases works", {
  testthat::expect_no_error({
    ojo_crim_cases()
  })
})
