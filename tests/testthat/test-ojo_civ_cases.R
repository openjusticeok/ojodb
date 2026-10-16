test_that("ojo_civ_cases works", {
  skip_if_no_db()

  expect_no_error({
    ojo_civ_cases()
  })
})
