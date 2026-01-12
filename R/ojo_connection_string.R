#' @title Create a Database Connection String
#'
#' @description Generates a standard connection string URI for various database
#'   backends from an `ojo_db` configuration object.
#'
#' @details This function is useful for applications or libraries that require a
#'   connection URI. It intelligently constructs the appropriate string based on
#'   the driver specified in the configuration.
#'
#'   - For **PostgreSQL** (`RPostgres`), it creates a `postgresql://` URI.
#'   - For **DuckDB** (`duckdb`), it creates a `duckdb://` URI, treating the
#'     `host` field in the config as the path to the database file.
#'
#' @param config A `db_config` object created by `db_config()`. If `NULL` (the
#'   default), a default configuration is created by calling `db_config()`,
#'   which will pull from environment variables.
#'
#' @return A single character string representing the database connection URI.
#' @export
#'
#' @examples
#' \dontrun{
#' # Create a PostgreSQL connection string from environment variables
#' ojo_connection_string(config = db_config(driver = "RPostgres"))
#'
#' # Create a connection string for a local SQLite database file
#' duckdb_config <- db_config(driver = "duckdb", host = "/path/to/db.duckdb")
#' ojo_connection_string(config = duckdb_config)
#' }
ojo_connection_string <- function(config = NULL) {
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

  # Dispatch to the appropriate internal function based on the driver
  connection_string_builder <- switch(
    config$driver,
    "RPostgres" = .create_postgres_string,
    "RSQLite" = .create_sqlite_string,
    "duckdb" = .create_duckdb_string,
    rlang::abort(glue::glue(
      "Connection string generation is not supported for the driver: '{config$driver}'."
    ))
  )

  # TODO: Return invisibly unless arg switch flipped
  connection_string_builder(config)
}

#' @title Create Postgres Connection String
#' @description Creates a PostgreSQL connection string.
#' @keywords internal
.create_postgres_string <- function(config) {
  # Check for required parameters
  required <- c("host", "port", "username", "password")
  missing_params <- setdiff(required, names(config))
  if (length(missing_params) > 0) {
    rlang::abort(
      c(
        "PostgreSQL config is missing required parameters to build a connection string.",
        "i" = glue::glue("Missing: {paste(missing_params, collapse = ', ')}")
      )
    )
  }

  # Build the base URI
  base_uri <- glue::glue(
    "postgresql://{config$username}:{config$password}@{config$host}:{config$port}/ojodb"
  )

  # Identify and collect query parameters (e.g., for SSL)
  param_keys <- c("sslmode", "sslrootcert", "sslcert", "sslkey")

  # Rename keys from db_config to match connection string params
  names(config)[names(config) == "ssl_mode"] <- "sslmode"
  names(config)[names(config) == "ssl_root_cert"] <- "sslrootcert"
  names(config)[names(config) == "ssl_cert"] <- "sslcert"
  names(config)[names(config) == "ssl_key"] <- "sslkey"

  query_params <- config[names(config) %in% param_keys]

  # Filter out any NULL or empty parameters
  query_params <- query_params[!sapply(query_params, is.null)]
  query_params <- query_params[query_params != ""]

  # If there are any parameters, format them into a query string
  if (length(query_params) > 0) {
    query_string <- paste(
      names(query_params),
      query_params,
      sep = "=",
      collapse = "&"
    )
    return(paste0(base_uri, "?", query_string))
  } else {
    return(base_uri)
  }
}

#' @title Create DuckDB Connection String
#' @description Creates a DuckDB connection string.
#' @keywords internal
.create_duckdb_string <- function(config) {
  # For DuckDB, the connection string is simply the path to the database file.
  # An empty path signifies an in-memory database.
  if (is.null(config$host)) {
    rlang::abort(
      c(
        "DuckDB config is missing the database file path.",
        "i" = "Please provide the path in the `host` argument of `db_config()`. For an in-memory database, use an empty string `''`."
      )
    )
  }

  # For DuckDB, the connection string is just the path.
  return(config$host)
}

#' @title Create SQLite Connection String
#' @description Creates a SQLite connection string.
#' @keywords internal
.create_sqlite_string <- function(config) {
  # For SQLite, the 'host' is treated as the file path.
  if (is.null(config$host) || config$host == "") {
    rlang::abort(
      c(
        "SQLite config is missing the database file path.",
        "i" = "Please provide the path in the `host` argument of `db_config()`."
      )
    )
  }

  # The connection string for SQLite is the protocol followed by the path.
  glue::glue("sqlite://{config$host}")
}
