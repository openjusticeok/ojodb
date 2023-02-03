test_that("name suggestions work correctly", {
  name1 <- "My Project"
  name2 <- "My-Project"
  name3 <- "my-project"
  name4 <- "my_projec!!t"
  name5 <- "my-pro.ject"

  expect_warning(val <- suggest_project_name(name1), "The project name `My Project` doesn't follow naming conventions. What about `my-project`?")
  expect_equal(suggest_project_name(name1), val)
  expect_warning(val <- suggest_project_name(name2), "The project name `My-Project` doesn't follow naming conventions. What about `my-project`?")
  expect_equal(suggest_project_name(name2), val)
  expect_no_error(suggest_project_name(name3))
  expect_equal(suggest_project_name(name3), name3)
  expect_warning(val <- suggest_project_name(name4), "The project name `my_projec!!t` doesn't follow naming conventions. What about `my_project`?")
  expect_equal(suggest_project_name(name4), val)
})
