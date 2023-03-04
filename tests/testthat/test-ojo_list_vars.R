test_that("isn't interactive", {
  expect_false(
    rlang::is_interactive()
  )
})

test_that("is interactive", {
  rlang::with_interactive({
    expect_true(
      rlang::is_interactive()
    )
  })
})

test_that("list vars fails in non-interactive", {
  withr::with_environment(
    env = rlang::new_environment(),
    code = expect_error(ojo_list_vars("case"))
  )
})

test_that("list vars works in interactive", {
  rlang::with_interactive(
    expect_silent(ojo_list_vars("case"))
  )
})
