test_that("ojo_connect works in interactive environment", {
  local(
    rlang::with_interactive({
      ojo_connect()
      expect_true(exists("ojodb"))
    })
  )
})
