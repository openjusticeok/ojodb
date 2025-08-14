#' Create Database Configuration
#'
#' @description
#' Creates a structured configuration object for database connections.
#' This function provides a standardized way to configure database connections
#' with support for different backends and authentication methods.
#'
#' @param backend The database backend to use. Options: "postgres", "duckdb"
#' @param ... Additional backend-specific configuration options
#' @param .admin Logical indicating whether to use admin credentials
#' @param .pool Logical indicating whether to create a connection pool
#'
#' @export
#' @returns A classed list containing database configuration
#'
#' @examples
#' \dontrun{
#' # Basic Postgres configuration
#' config <- db_config("postgres")
#' 
#' # Admin Postgres configuration with pooling
#' admin_config <- db_config("postgres", .admin = TRUE, .pool = TRUE)
#' 
#' # DuckDB configuration
#' duck_config <- db_config("duckdb")
#' }
#'
db_config <- function(backend = "postgres", ..., .admin = FALSE, .pool = FALSE) {
  config <- list(
    backend = backend,
    admin = .admin,
    pool = .pool,
    extra_args = list(...)
  )
  
  class(config) <- c("db_config", "list")
  config
}

#' @export
print.db_config <- function(x, ...) {
  cat("Database Configuration\n")
  cat("  Backend:", x$backend, "\n")
  cat("  Admin:", x$admin, "\n")
  cat("  Pool:", x$pool, "\n")
  if (length(x$extra_args) > 0) {
    cat("  Extra args:", paste(names(x$extra_args), collapse = ", "), "\n")
  }
  invisible(x)
}
