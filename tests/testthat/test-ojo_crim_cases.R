test_that("ojo_crim_cases works", {
  skip_if_no_db()

  expect_no_error({
    ojo_crim_cases()
  })
})
