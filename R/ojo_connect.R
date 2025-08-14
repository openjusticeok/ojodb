#' @title OJO Connect
#'
#' @description Connect to the Open Justice Oklahoma database
#'
#' @details
#' Opens a connection to the Open Justice Oklahoma database using credentials stored in the .Renviron file.
#' If no credentials exist, prompts for user, password, and host name and provides instructions to store them for future sessions.
#' 
#' This function can be used in two ways:
#' 1. **Environment-managed connections** (default): Creates a connection that is automatically managed and cleaned up
#' 2. **Local connections** (with `.local = TRUE`): Returns a connection object without side effects for manual management
#'
#' @param config A db_config object created with `db_config()`. If NULL, uses other parameters.
#' @param .admin A logical value indicating whether to connect to the database as an administrator.
#' @param ... Additional arguments passed to the database driver.
#' @param .driver The driver to use for the connection. Default is "RPostgres". "duckdb" is also supported.
#' @param .global Deprecated. A connection will always be created in the specified environment, or in the package environment by default.
#' @param .env The environment in which you want the connection stored (only used when .local = FALSE).
#' @param .pool A logical value indicating whether to use a connection pool from the `{pool}` package, or not.
#' @param .local A logical value indicating whether to return a local connection without environment assignment or deferred cleanup.
#'
#' @export
#' @returns A database connection object created with `RPostgres::Postgres()` and either `pool::dbPool` or `DBI::dbConnect`
#'
#' @examples
#' \dontrun{
#' # Environment-managed connection (default behavior)
#' ojo_connect()
#' 
#' # Local connection for manual management
#' con <- ojo_connect(.local = TRUE)
#' # ... use connection ...
#' DBI::dbDisconnect(con)
#' 
#' # Using structured configuration
#' config <- db_config("postgres", .admin = TRUE, .pool = TRUE)
#' ojo_connect(config)
#' 
#' # DuckDB backend
#' ojo_connect(.driver = "duckdb")
#' }
#' @section Side Effects:
#' When `.local = FALSE` (default), a connection object (named `ojo_con` or `ojo_pool` depending on the `.pool` argument) is created in the package environment and automatically cleaned up.
#' When `.local = TRUE`, no side effects occur.
#'
#' @seealso ojo_auth(), db_config()
#'
ojo_connect <- function(config = NULL, ..., .admin = FALSE, .driver = "RPostgres", .global = lifecycle::deprecated(), .env = ojo_env(), .pool = FALSE, .local = FALSE) {

  if (lifecycle::is_present(.global)) {
    lifecycle::deprecate_warn(
      when = "2.8.0",
      what = "ojo_connect(.global)"
    )
  }

  # Handle config object if provided
  if (!is.null(config)) {
    if (!inherits(config, "db_config")) {
      rlang::abort("config must be a db_config object created with db_config()")
    }
    # Config provides defaults, explicit parameters override
    if (.driver == "RPostgres" && config$backend != "RPostgres") {
      .driver <- config$backend
    }
    if (!.admin && config$admin) {
      .admin <- config$admin
    }
    if (!.pool && config$pool) {
      .pool <- config$pool
    }
    # Merge extra args from config with ... args (... takes precedence)
    extra_args <- config$extra_args
  } else {
    extra_args <- list()
  }
  
  # Combine arguments
  all_args <- c(list(...), extra_args)

  # For local connections, skip environment management
  if (.local) {
    return(create_connection(.driver, .admin, .pool, all_args))
  }

  # Environment-managed connection logic (existing behavior)
  connection_type <- if (.pool) "ojo_pool" else "ojo_con"
  connection_key <- paste0(connection_type, "_", .driver)

  # Check if a valid connection object already exists in the environment
  existing_conn <- get_connection_object(.env, connection_key)
  if (!is.null(existing_conn) && DBI::dbIsValid(existing_conn)) {
    return(existing_conn)
  }

  # Create new connection
  new_conn <- create_connection(.driver, .admin, .pool, all_args)
  assign(connection_key, new_conn, envir = .env)

  # Setup deferred cleanup
  withr::defer({
    if (exists(connection_key, envir = .env)) {
      connection_object <- get(connection_key, envir = .env, inherits = FALSE)
      if (.pool) {
        pool::poolClose(connection_object)
      } else {
        DBI::dbDisconnect(connection_object)
      }
      rm(list = connection_key, envir = .env)
    }
  }, envir = .env)

  return(new_conn)
}

#' Create Database Connection
#'
#' @description
#' Internal function to create a database connection using the specified backend configuration.
#'
#' @param .driver The database driver to use
#' @param .admin Whether to use admin credentials
#' @param .pool Whether to create a connection pool
#' @param extra_args Additional arguments for the connection
#'
#' @keywords internal
#' @returns A database connection object
#'
create_connection <- function(.driver, .admin, .pool, extra_args) {
  # Get backend-specific configuration
  conn_args <- switch(
    .driver,
    "RPostgres" = do.call(postgres_config, c(list(.admin = .admin), extra_args)),
    "duckdb" = do.call(duckdb_config, extra_args),
    rlang::abort("Unsupported driver: {.driver}")
  )

  # Choose connection function
  conn_fn <- if (.pool) pool::dbPool else DBI::dbConnect

  # Create connection
  new_conn <- rlang::exec(conn_fn, !!!conn_args)

  # Backend-specific setup
  if (.driver == "duckdb") {
    setup_duckdb_features(new_conn)
  }

  return(new_conn)
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
get_connection_object <- function(env, key) {
  connection_object_exists <- exists(
    key,
    envir = env,
    inherits = FALSE
  )

  if (!connection_object_exists) {
    return(NULL)
  }

  connection_object <- get(
    key,
    envir = env,
    inherits = FALSE
  )

  return(connection_object)
}
