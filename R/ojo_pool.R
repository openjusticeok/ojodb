#' @title Create a Database Connection Pool
#'
#' @description Creates a managed pool of database connections using the \{pool\}
#'   package. This is the recommended way to connect to the database for
#'   applications with concurrent users, such as Shiny apps.
#'
#' @details A connection pool is more efficient than managing individual
#'   connections in a concurrent environment. Instead of creating and tearing
#'   down a new connection for each user or session, the pool maintains a set of
#'   active connections that are "checked out" as needed and returned to the
#'   pool when the operation is complete.
#'
#'   This function is a dedicated wrapper around `ojo_connect()` that forces the
#'   creation of a pool.
#'
#'   **Important:** Remember to close the pool with `pool::poolClose(pool)` when
#'   your application shuts down to release all database connections.
#'
#' @param config A `db_config` object created by `db_config()`. If `NULL` (the
#'   default), a default configuration is created, which will pull from
#'   environment variables.
#'
#' @param ... Additional arguments passed on to the underlying `pool::dbPool()`
#'   function, such as `minSize`, `maxSize`, or `idleTimeout`.
#'
#' @return A `Pool` object.
#' @export
#'
#' @examples
#' \dontrun{
#' # Create a connection pool using default settings
#' pool <- ojo_pool()
#'
#' # Use the pool with ojo_tbl or dplyr
#' ojo_tbl("case", con = pool)
#'
#' # When your application stops, close the pool
#' pool::poolClose(pool)
#'
#' # Create a pool for a specific backend (e.g., SQLite)
#' sqlite_config <- db_config(.driver = "RSQLite", host = "local.db")
#' sqlite_pool <- ojo_pool(config = sqlite_config)
#' pool::poolClose(sqlite_pool)
#' }
ojo_pool <- function(config = NULL, ...) {
  # If no config is provided, create a default one.
  if (is.null(config)) {
    config <- db_config()
  }

  # Ensure the config object is valid
  if (!inherits(config, "db_config")) {
    rlang::abort(
      "`config` must be a `db_config` object created by `db_config()`."
    )
  }

  # Check if the selected driver supports connection pooling
  supported_drivers <- c("RPostgres", "RSQLite")
  if (!config$driver %in% supported_drivers) {
    rlang::abort(
      glue::glue(
        "Connection pooling is not supported for the '{config$driver}' driver."
      ),
      "i" = glue::glue(
        "Supported drivers are: {paste(supported_drivers, collapse = ', ')}."
      )
    )
  }

  # Assemble the arguments for the pool::dbPool function based on the driver.
  # This logic is similar to ojo_connect's internals but targets pool::dbPool.
  conn_args <- switch(
    config$driver,
    "RPostgres" = list(
      drv = RPostgres::Postgres(),
      dbname = config$database,
      host = config$host,
      port = as.integer(config$port),
      user = config$username,
      password = config$password,
      sslmode = config$ssl_mode,
      sslrootcert = config$ssl_root_cert,
      sslcert = config$ssl_cert,
      sslkey = config$ssl_key,
      bigint = "integer"
    ),
    "RSQLite" = list(
      drv = RSQLite::SQLite(),
      # TODO: Decide whether host or database should be the file path
      dbname = config$host # For SQLite, 'host' is the file path
    )
  )

  # Combine the generated args with any additional args passed via ...
  final_args <- c(conn_args, list(...))

  # Create the connection pool using the assembled arguments
  rlang::exec(pool::dbPool, !!!final_args)
}
