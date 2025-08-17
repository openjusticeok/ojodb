#' @title Connect to an OJO Database Backend
#'
#' @description Establishes a single connection to a database, such as the Open
#'   Justice Oklahoma Postgres database or a local DuckDB instance.
#'
#' @details This function serves as the primary way to create individual database
#'   connection objects. It is designed to be side-effect-free, returning the
#'   connection object directly to the user for manual management. For creating
#'   a managed pool of connections suitable for applications, use `ojo_pool()`.
#'
#' @param db_config A `db_config` object created by `db_config()`. If `NULL` (the
#'   default), a default configuration is created by calling `db_config()` with
#'   no arguments, which will pull from environment variables.
#' @param ... Placeholder for future arguments.
#'
#' @return A database connection object (e.g., a `PqConnection` or `duckdb_connection`).
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # --- Manual Connection Management ---
#'
#' # 1. Create a configuration
#' my_config <- db_config(.admin = TRUE)
#'
#' # 2. Create a connection object
#' con <- ojo_connect(db_config = my_config)
#'
#' # 3. Use the connection with dplyr or DBI
#' dplyr::tbl(con, "case")
#' DBI::dbListTables(con)
#'
#' # 4. Close the connection when finished
#' DBI::dbDisconnect(con)
#' }
#' @seealso [db_config()] for creating configuration objects, and [ojo_pool()]
#'   for creating a connection pool.
ojo_connect <- function(db_config = NULL, ...) {
  # If no config is provided, create a default one.
  if (is.null(db_config)) {
    db_config <- db_config()
  }

  # Ensure the config object is valid
  if (!inherits(db_config, "db_config")) {
    rlang::abort(
      "`db_config` must be a `db_config` object created by `db_config()`."
    )
  }

  if (is.null(db_config$driver) || db_config$driver == "") {
    rlang::abort(
      c(
        "Database configuration is missing required parameters.",
        "i" = glue::glue("Missing: {paste(missing_params, collapse = ', ')}"),
        "*" = "Please set them with `ojo_auth()` or in your `db_config()` call."
      )
    )
  }

  # Dispatch to the appropriate backend-specific connection function
  connection_function <- switch(
    db_config$driver,
    "RPostgres" = .connect_postgres,
    "duckdb" = .connect_duckdb,
    "RSQLite" = .connect_sqlite,
    rlang::abort(glue::glue(
      "The driver '{db_config$driver}' is not supported."
    ))
  )

  # Call the selected connection function
  connection_function(db_config, ...)
}

#' Internal function to connect to Postgres
#' @keywords internal
.connect_postgres <- function(db_config, ...) {
  # Check for required parameters
  required <- c("database", "host", "port", "username", "password", "ssl_mode")
  missing_params <- setdiff(required, names(db_config))
  if (length(missing_params) > 0) {
    rlang::abort(
      c(
        "Postgres connection is missing required configuration parameters.",
        "i" = glue::glue("Missing: {paste(missing_params, collapse = ', ')}"),
        "*" = "Please set them with `ojo_auth()` or in your `db_config()` call."
      )
    )
  }

  # Assemble the arguments for the connection function
  conn_args <- list(
    drv = RPostgres::Postgres(),
    dbname = db_config$database,
    host = db_config$host,
    port = as.integer(db_config$port),
    user = db_config$username,
    password = db_config$password,
    sslmode = db_config$ssl_mode,
    sslrootcert = db_config$ssl_root_cert,
    sslcert = db_config$ssl_cert,
    sslkey = db_config$ssl_key,
    bigint = "integer",
    check_interrupts = TRUE
  )

  # Create the connection
  rlang::exec(DBI::dbConnect, !!!conn_args)
}

#' Internal function to connect to DuckDB
#' @keywords internal
.connect_duckdb <- function(db_config, ...) {
  # Establish the connection (in-memory by default)
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")

  # Install and load necessary extensions for accessing remote data
  tryCatch(
    {
      DBI::dbExecute(con, "INSTALL httpfs; LOAD httpfs;")
      DBI::dbExecute(con, "SET s3_endpoint='storage.googleapis.com';")
    },
    error = function(e) {
      rlang::warn(c(
        "Failed to install or configure DuckDB extensions.",
        "i" = "Accessing remote data (e.g., from GCS) may not work.",
        "x" = e$message
      ))
    }
  )

  con
}

#' Internal function to connect to SQLite
#' @keywords internal
.connect_sqlite <- function(db_config, ...) {
  # For SQLite, the 'host' is treated as the file path.
  if (is.null(db_config$host) || db_config$host == "") {
    rlang::abort(
      c(
        "SQLite config is missing the database file path.",
        "i" = "Please provide the path in the `host` argument of `db_config()`."
      )
    )
  }

  DBI::dbConnect(RSQLite::SQLite(), dbname = db_config$host)
}

#' @title Get or create the default OJO database connection
#'
#' @description This internal function manages a single, default connection object
#'   stored in the package's private environment (`.ojo_env`). It is the
#'   cornerstone of the interactive user experience, ensuring all default
#'   database operations for a given backend share the same connection.
#'
#' @details
#' This function accepts arguments (...) that are passed to `ojo_connect()`,
#' allowing it to manage distinct default connections for different backends.
#'
#' The first time this function is called with a unique configuration, it will:
#' 1. Call `ojo_connect()` with the provided arguments.
#' 2. Store the connection in the `.ojo_env` environment under a unique key.
#' 3. Use `withr::defer()` to register a cleanup handler for that connection.
#'
#' @param ... Arguments to pass to `ojo_connect()`, primarily a `db_config` object
#'   to specify the backend (e.g., `db_config = db_config(.driver = "duckdb")`).
#'
#' @return A valid database connection object.
#' @keywords internal
ojo_default_connection <- function(...) {
  # Capture the arguments to create a unique fingerprint for the connection type.
  args <- list(...)
  connection_key <- paste0("default_con_", digest::digest(args))

  # Check if a valid connection for this specific configuration already exists
  if (exists(connection_key, envir = .ojo_env)) {
    con <- get(connection_key, envir = .ojo_env)
    if (DBI::dbIsValid(con)) {
      return(con)
    }
  }

  # If no valid connection exists, create a new one.
  cli::cli_inform(
    c(
      "i" = "Creating a new default connection to the OJO database.",
      "*" = "This connection will be closed automatically when your R session ends."
    ),
    .frequency = "once",
    .frequency_id = "ojo_connect_inform"
  )

  new_con <- rlang::exec(ojo_connect, !!!args)

  # Store the new connection in the package environment under its unique key.
  assign(connection_key, new_con, envir = .ojo_env)

  # Register a deferred event in the global environment to ensure cleanup
  # happens when the user's session ends.
  withr::defer(
    {
      if (exists(connection_key, envir = .ojo_env)) {
        con_to_close <- get(connection_key, envir = .ojo_env)
        if (DBI::dbIsValid(con_to_close)) {
          # This handles single connections only now
          DBI::dbDisconnect(con_to_close)
        }
        rm(list = connection_key, envir = .ojo_env)
      }
    },
    envir = globalenv()
  )

  return(new_con)
}
