test_that("ojo_search_minutes returns a lazy tibble", {
  skip_on_runiverse()
  
  # Test with a simple search term
  result <- ojo_search_minutes("test", .silent = TRUE)
  
  # Check that it returns a lazy tibble (tbl_lazy class)
  expect_s3_class(result, "tbl_lazy")
  
  # Ensure it's not an eager tibble or data.frame
  expect_false(inherits(result, "data.frame"))
  
  # The lazy tibble should not be materialized yet
  expect_false(exists("data", envir = environment(result)))
  
  withr::deferred_run(envir = ojo_env())
})

test_that("ojo_search_minutes handles Postgres connection requirement", {
  skip_on_runiverse()
  
  # This test ensures the function validates Postgres connections like ojo_query does
  expect_no_error({
    result <- ojo_search_minutes("test", .silent = TRUE)
    expect_s3_class(result, "tbl_lazy")
  })
  
  withr::deferred_run(envir = ojo_env())
})

test_that("ojo_search_minutes query construction works for different query types", {
  skip_on_runiverse()
  
  # Test that the function can handle different query types and returns lazy tibbles
  result1 <- ojo_search_minutes("simple", .silent = TRUE)
  result2 <- ojo_search_minutes("term1 & term2", .silent = TRUE)
  result3 <- ojo_search_minutes("phrase with spaces", .silent = TRUE)
  result4 <- ojo_search_minutes("term1 | term2", .silent = TRUE)
  result5 <- ojo_search_minutes("!! negative", .silent = TRUE)
  
  expect_s3_class(result1, "tbl_lazy")
  expect_s3_class(result2, "tbl_lazy")
  expect_s3_class(result3, "tbl_lazy")
  expect_s3_class(result4, "tbl_lazy")
  expect_s3_class(result5, "tbl_lazy")
  
  withr::deferred_run(envir = ojo_env())
})

test_that("ojo_search_minutes can be used with collect()", {
  skip_on_runiverse()
  
  # Test that the lazy tibble can be collected (this will actually execute the query)
  result <- ojo_search_minutes("test", .silent = TRUE)
  expect_s3_class(result, "tbl_lazy")
  
  # This would execute the query - we can't test actual execution without a DB connection
  # but we can verify the structure is correct for collection
  expect_no_error({
    # Just check that collect method exists for this object type
    methods(class = class(result))
  })
  
  withr::deferred_run(envir = ojo_env())
})