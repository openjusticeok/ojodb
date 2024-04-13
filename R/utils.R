#' @title OJO Environment
#'
#' @description Get the environment of the OJO package, which holds the database pool object.
#'
#' @param ... Placeholder for future arguments
#' 
#' @export
#'
ojo_env <- function(...) {
  .ojo_env
}

#' @title OJO Version
#'
#' @description
#' Returns the version of the `ojodb` package.
#'
#' @param ... Placeholder for future arguments
#'
#' @export
#' @returns A character vector containing the version number of the `ojodb` package.
#' 
ojo_version <- function(...) {
  utils::packageVersion("ojodb")
}

#' @title Skip If No Database Connection
#' 
#' @description
#' Skips a test if the `ojodb` package is not installed or if the user is not connected to the OJO database.
#'
#' @keywords internal
#' 
skip_if_no_db <- function() {
  testthat::skip("No database connection available")
}

#' @title Get Connection Object
#' 
#' @description
#' Gets the connection object from the environment specified by the `.env` argument.
#' 
#' @param env The environment to search for the connection object.
#' 
#' @keywords internal
#' 
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