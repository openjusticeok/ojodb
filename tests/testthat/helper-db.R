skip_if_no_db <- function() {
  can_connect <- tryCatch(
    {
      con <- ojo_connect()
      DBI::dbDisconnect(con)
      TRUE
    },
    error = function(e) {
      FALSE
    }
  )

  if (!can_connect) {
    testthat::skip("Database connection failed")
  }
}
