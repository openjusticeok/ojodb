#' @title Skip a test if no database connection is available
#'
#' @description This is a helper function for {testthat} that attempts to create
#'   a connection to the database. If the connection fails for any reason
#'   (e.g., missing credentials in the environment, network issues), it will
#'   gracefully skip the test.
#'
#' @details This function is essential for writing tests that can run in different
#'   environments. On a local machine with credentials, the tests will run against
#'   the live database. On a CI/CD server without credentials, the tests will be
#'   skipped instead of failing.
#'
#' @keywords internal
#' @export
skip_if_no_db <- function() {
  tryCatch({
    con <- ojo_connect()
    DBI::dbDisconnect(con)
  }, error = function(e) {
    testthat::skip("Could not connect to the database. Skipping test.")
  })
}
