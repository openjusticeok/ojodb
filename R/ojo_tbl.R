#' @title Get a table from a database connection
#'
#' @description Creates a lazy tibble from a table in a database. This is the
#'   primary way to begin a database query with {ojodb}.
#'
#' @details For interactive use, if no connection is provided, this function will
#'   automatically create and manage a default connection to the primary OJO
#'   Postgres database. To connect to other backends (like DuckDB or SQLite),
#'   you must first create a connection object with `ojo_connect()` and pass it
#'   via the `con` argument.
#'
#' @param table The name of the table to query.
#' @param schema The name of the schema where the table resides.
#' @param con A database connection object. If `NULL` (the default), a managed
#'   default connection will be created and used automatically.
#'
#' @export
#' @return A lazy tibble with the `ojo_tbl` class.
#'
#' @examples
#' \dontrun{
#' # Interactively connect to the 'case' table in the default Postgres DB
#' case_tbl <- ojo_tbl("case")
#'
#' # Connect to a table on a manually created DuckDB connection
#' my_duckdb_con <- ojo_connect(db_config = db_config(.driver = "duckdb"))
#' my_duckdb_tbl <- ojo_tbl("some_table", con = my_duckdb_con)
#' }
ojo_tbl <- function(table, schema = "public", con = NULL) {
  # If no connection is provided, get the default singleton connection.
  # This defaults to the primary Postgres database.
  if (is.null(con)) {
    con <- ojo_default_connection()
  }

  # Create a lazy tibble from the connection
  data <- dplyr::tbl(con, DBI::Id(schema = schema, table = table))

  class(data) <- c("ojo_tbl", class(data))
  return(data)
}
