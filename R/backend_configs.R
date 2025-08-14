#' PostgreSQL Backend Configuration
#'
#' @description
#' Creates connection arguments for PostgreSQL backend using ojo_auth configuration.
#'
#' @param .admin Logical indicating whether to use admin credentials
#' @param ... Additional arguments passed to RPostgres::Postgres()
#'
#' @keywords internal
#' @returns List of connection arguments for PostgreSQL
#'
postgres_config <- function(.admin = FALSE, ...) {
  user_type <- if (.admin) "ADMIN" else "DEFAULT"
  
  # Check if required environment variables exist
  if (Sys.getenv("OJO_HOST") == "") {
    rlang::abort(
      "No {tolower(user_type)} configuration for the OJO database was found. Please create one now using `ojo_auth`, or manually, by adding the necessary environment variables with `usethis::edit_r_environ`.",
      use_cli_format = TRUE
    )
  }
  
  list(
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
  )
}

#' DuckDB Backend Configuration
#'
#' @description
#' Creates connection arguments for DuckDB backend.
#'
#' @param ... Additional arguments passed to duckdb::duckdb()
#'
#' @keywords internal
#' @returns List of connection arguments for DuckDB
#'
duckdb_config <- function(...) {
  list(
    drv = duckdb::duckdb(),
    ...
  )
}

#' Setup DuckDB Features
#'
#' @description
#' Configures DuckDB instance with required plugins and settings.
#'
#' @param conn DuckDB connection object
#'
#' @keywords internal
#' @returns NULL (called for side effects)
#'
setup_duckdb_features <- function(conn) {
  DBI::dbExecute(
    conn,
    stringr::str_glue("INSTALL httpfs; LOAD httpfs; SET s3_endpoint='storage.googleapis.com';")
  )
}