test_that("environment variables are visible in the test environment", {
  # Print the value of the environment variable to the console
  print(paste("OJO_HOST in test environment is:", Sys.getenv("OJO_HOST")))

  # The test will fail if the variable is not set, 
  # but the print statement above will still run.
  expect_true(
    Sys.getenv("OJO_HOST") != "", 
    label = "OJO_HOST environment variable was not found."
  )
})
