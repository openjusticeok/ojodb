#' @title OJO Connect
#'
#' @description Connect to the Open Justice Oklahoma database
#'
#' @details
#' Opens a connection to the Open Justice Oklahoma database using credentials stored in the .Renviron file.
#' If no credentials exist, prompts for user, password, and host name and provides instructions to store them for future sessions.
#'
#' @param .admin A logical value indicating whether to connect to the database as an administrator
#' @param ... Placeholder
#' @param .global A logical value indicating whether to establish the connection in the global environment or not.
#' @param .env The environment in which you want the connection stored
#' @param .pool A logical value indicating whether to use a connection pool or not.
#'
#' @export
#' @returns A database connection object created with `pool::dbPool` and `RPostgres::Postgres()`
#'
#' @examples
#' \dontrun{
#' ojo_connect()
#' }
#' @section Side Effects:
#' If either the `.global` argument or `rlang::is_interactive` are `TRUE`, an object named `ojo_pool` is created in the package environment.
#'
#' @seealso ojo_auth()
#'
ojo_connect <- function(..., .admin = FALSE, .global = rlang::is_interactive(), .env = ojo_env(), .pool = FALSE) {

  user_type <- if (.admin) "ADMIN" else "DEFAULT"

  if (Sys.getenv("OJO_HOST") == "") {
    rlang::abort(
      "No {tolower(user_type)} configuration for the OJO database was found. Please create one now using `ojo_auth`, or manually, by adding the necessary environment variables with `usethis::edit_r_environ`.",
      use_cli_format = TRUE
    )
  }

  connection_type <- if (.pool) "ojo_pool" else "ojo_con"

  # Check if object with correct connection type and user already exists and is valid
  if (.global && exists(connection_type, envir = .env)) {
    db <- get(connection_type, envir = .env, inherits = FALSE)

    db_is_valid <- switch(
      connection_type,
      ojo_pool = pool::dbIsValid(db),
      ojo_con = DBI::dbIsValid(db)
    )

    if (db_is_valid) {
      return(db)
    }
  }

  conn_args <- list(
    drv = RPostgres::Postgres(),
    dbname = "ojodb",
    host = Sys.getenv("OJO_HOST"),
    port = Sys.getenv("OJO_PORT"),
    user = Sys.getenv(glue::glue("OJO_{user_type}_USER")),
    password = Sys.getenv(glue::glue("OJO_{user_type}_PASS")),
    sslmode = Sys.getenv("OJO_SSL_MODE"),
    sslrootcert = Sys.getenv("OJO_SSL_ROOT_CERT"),
    sslcert = Sys.getenv("OJO_SSL_CERT"),
    sslkey = Sys.getenv("OJO_SSL_KEY"),
    bigint = "integer",
    ...
  )

  conn_fn <- switch(
    connection_type,
    ojo_pool = pool::dbPool,
    ojo_con = DBI::dbConnect
  )

  conn <- rlang::exec(conn_fn, !!!conn_args)

  if (.global) {
    assign(connection_type, conn, envir = .env)
    withr::defer(
      {
        if(exists(connection_type, envir = .env)) {
          pool::poolClose(
            get(connection_type, envir = .env, inherits = FALSE)
          )
          rm(list = connection_type, envir = .env)
        }
      },
      envir = .env
    )
    invisible()
  }

  invisible(conn)
}
