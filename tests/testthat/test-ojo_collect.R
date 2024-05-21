test_that("ojo_collect works in interactive mode", {
  expect_no_error({
    ojo_tbl("case") |>
      head(1) |>
      ojo_collect()
  })

  expect_s3_class(
    ojo_tbl("case") |>
      head(1) |>
      ojo_collect(),
    "tbl_df"
  )

  withr::deferred_run(envir = ojo_env())
})
