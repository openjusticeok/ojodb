skip_if_no_db <- function() {
  testthat::skip("No database connection available")
}

get_connection_object <- function(env) {
  pool_object_exists <- exists("ojo_pool", envir = env, inherits = FALSE)
  con_object_exists <- exists("ojo_con", envir = env, inherits = FALSE)

  if (pool_object_exists && con_object_exists) {
    rlang::abort(
      "Both a connection pool and a connection object exist in the environment specified by the `.env` argument. Please remove one of them, set `.global = FALSE`, or restart your R session before continuing.",
      use_cli_format = TRUE
    )
  } else if (!pool_object_exists && !con_object_exists) {
    return(NULL)
  }

  connection_type <- if (pool_object_exists) "ojo_pool" else "ojo_con"

  connection_object <- get(connection_type, envir = env, inherits = FALSE)

  return(connection_object)
}