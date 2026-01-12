
# Explicitly load .Renviron if it exists in home directory
# This fixes issues where devtools::test() might not load it automatically
home_renv <- file.path(Sys.getenv("HOME"), ".Renviron")
if (file.exists(home_renv)) {
  readRenviron(home_renv)
}

skip_if_no_db <- function() {
  can_connect <- tryCatch(
    {
      con <- ojo_connect()
      DBI::dbDisconnect(con)
      TRUE
    },
    error = function(e) {
      message("DEBUG: Connection failed: ", e$message)
      FALSE
    }
  )

  if (!can_connect) {
    testthat::skip("Database connection failed")
  }
}
