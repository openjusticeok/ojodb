test_that("ojo_collect works in interactive mode", {
  rlang::with_interactive({
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
  })
})
