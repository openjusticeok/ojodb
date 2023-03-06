test_that("ojo_list_vars succeeds correctly in non-interactive mode", {
  db <- ojo_connect()

  d <- ojo_list_vars("case", .con = db)

  expect_true(inherits(d, "tbl_Pool"))

  pool::poolClose(db)

  expect_false(pool::dbIsValid(db))
})

test_that("ojo_list_vars succeeds correctly in interactive mode", {
  
  rlang::with_interactive({
    ojo_list_vars("case") |>
      dplyr::collect() |>
      capture_output()

    expect_true(pool::dbIsValid(ojo_pool))
  })
})