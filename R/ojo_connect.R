#' @title OJO Connect
#'
#' @description Connect to the Open Justice Oklahoma database
#'
#' @details
#' Opens a connection to the Open Justice Oklahoma database using credentials stored in the .Renviron file.
#' If no credentials exist, prompts for user, password, and host name and provides instructions to store them for future sessions.
#'
#' @param .admin A logical value indicating whether to connect to the database as an administrator.
#' @param ... Placeholder.
#' @param .driver The driver to use for the connection. Default is "RPostgres". "duckdb" is also supported.
#' @param .global Deprecated. A connection will always be created in the specified environment, or in the package environment by default.
#' @param .env The environment in which you want the connection stored.
#' @param .pool A logical value indicating whether to use a connection pool from the `{pool}` package, or not.
#'
#' @export
#' @returns A database connection object created with `RPostgres::Postgres()` and either `pool::dbPool` or `DBI::dbConnect`
#'
#' @examples
#' \dontrun{
#' ojo_connect()
#' }
#' @section Side Effects:
#' A connection object (named `ojo_con` or `ojo_pool` depending on the `.pool` argument) is created in the package environment.
#'
#' @seealso ojo_auth()
#'
ojo_connect <- function(..., .admin = FALSE, .driver = "RPostgres", .global = lifecycle::deprecated(), .env = ojo_env(), .pool = FALSE) {

  if (lifecycle::is_present(.global)) {
    lifecycle::deprecate_warn(
      when = "2.8.0",
      what = "ojo_connect(.global)"
    )
  }

  user_type <- if (.admin) "ADMIN" else "DEFAULT"

  if (Sys.getenv("OJO_HOST") == "" && .driver == "RPostgres") {
    rlang::abort(
      "No {tolower(user_type)} configuration for the OJO database was found. Please create one now using `ojo_auth`, or manually, by adding the necessary environment variables with `usethis::edit_r_environ`.",
      use_cli_format = TRUE
    )
  }

  connection_type <- if (.pool) "ojo_pool" else "ojo_con"

  # Check if a valid connection object already exists in the environment
  existing_conn <- get_connection_object(.env)
  if (!is.null(existing_conn) && DBI::dbIsValid(existing_conn)) {
    return(existing_conn)
  }

  # Set the driver and connection arguments
  conn_args <- switch(
    .driver,
    "RPostgres" = list(
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
      check_interrupts = TRUE,
      ...
    ),
    "duckdb" = list(
      drv = duckdb::duckdb(),
      ...
    ),
    rlang::abort("Unsupported driver: {.driver}")
  )

  conn_fn <- switch(
    connection_type,
    ojo_pool = pool::dbPool,
    ojo_con = DBI::dbConnect
  )

  new_conn <- rlang::exec(conn_fn, !!!conn_args)
  assign(connection_type, new_conn, envir = .env)

  # Make sure duckdb instance has needed features
  if (.driver == "duckdb") {
    DBI::dbExecute(new_conn, stringr::str_glue("INSTALL httpfs; LOAD httpfs; SET s3_endpoint='storage.googleapis.com';"))
  }

  withr::defer({
    if (exists(connection_type, envir = .env)) {
      connection_object <- get(connection_type, envir = .env, inherits = FALSE)
      if (.pool) {
        pool::poolClose(connection_object)
      } else {
        DBI::dbDisconnect(connection_object)
      }
      rm(list = connection_type, envir = .env)
    }
  }, envir = .env)

  return(new_conn)
}
