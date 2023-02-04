test_that("creating a project works", {
  withr::with_tempdir({
    expect_no_error(ojo_create_project("test_project"))
    # expect_true(fs::dir_exists("./test/"))
    # expect_true(fs::file_exists("./test/test.txt"))
  })
})
