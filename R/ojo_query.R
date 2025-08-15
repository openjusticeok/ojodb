#' @title Send a raw SQL query to the database
#'
#' @description Executes a raw SQL query against a database connection. This
#'   function is backend-agnostic and will send the query to any valid DBI
#'   connection.
#'
#' @param query The SQL query to send to the database. The user is responsible
#'   for ensuring the syntax is correct for the target database backend.
#' @param con The database connection to use. If `NULL`, a default connection
#'   to the primary Postgres database will be created and used.
#'
#' @export
#' @returns A lazy tibble with the `ojo_tbl` class.
#'
#' @examples
#' \dontrun{
#' # Run a query against the default Postgres database
#' ojo_query("SELECT * FROM case LIMIT 10")
#'
#' # Run a query against a manual DuckDB connection
#' duck_con <- ojo_connect(db_config = db_config(.driver = "duckdb"))
#' ojo_query("SELECT * FROM 'my_duck_db_file.parquet' LIMIT 5", con = duck_con)
#' }
ojo_query <- function(query, con = NULL) {
  # If no connection is provided, get the default singleton connection.
  if (is.null(con)) {
    con <- ojo_default_connection()
  }

  # Create a lazy tibble from the raw SQL query
  data <- dplyr::tbl(con, dbplyr::sql(query))

  # Add the ojo_tbl class for consistency with ojo_tbl()
  class(data) <- c("ojo_tbl", class(data))
  return(data)
}
