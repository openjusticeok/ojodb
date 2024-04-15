#' Identify a table from the OJO database
#'
#' Identifies a table in the OJO database from which to query data. Remember to run `connect_ojo()` to establish a connection before attempting to query and to close the connection afterwards with `disconnect_ojo()`.
#'
#' @aliases ojo_tbl
#'
#' @param table The name of a table in the OJO database. To get a list of tables, run `ojo_list_tables()`
#' @param schema The name of a schema in the OJO database. To get a list of schemas, run `ojo_list_schemas()`
#' @param ... Placeholder
#' @param .con The ojodb connection to use
#' @param .source `r lifecycle::badge("experimental")` The source of the table. Options are 'database' and 'gcs'. Default is 'database'.
#'
#' @export ojo_tbl
#' @return A pointer to a table that can be passed to dplyr functions and/or pulled into a dataframe using `ojo_collect()`
#' @examples
#' \dontrun{
#' # Identifies the table
#' ojo_tbl("case")
#'
#' # Pulls down case information data for every Tulsa felony filed in 2020 into a dataframe d
#' d <- ojo_tbl("case") %>%
#'   filter(district == "TULSA", case_type == "CF", year == 2020) %>%
#'   collect()
#' }
#' @seealso ojo_list_tables(), ojo_list_vars(), ojo_list_schemas()
#'
ojo_tbl <- function(
  table,
  schema = "public",
  ...,
  .con = NULL,
  .source = "database"
) {
  if (.source == "database") {
    if (is.null(.con)) {
      .con <- ojo_connect(...)
    }
    data <- tbl_from_database(.con, schema, table)
  } else if (.source == "gcs") {
    # Temp fix for schema
    if (schema == "public") {
      schema <- "oscn"
    }
    data <- tbl_from_gcs(schema, table)
  } else {
    rlang::abort("Invalid source specified. Please choose one of: 'database' or 'gcs'.")
  }

  class(data) <- c("ojo_tbl", class(data))

  return(data)
}

# Placeholder function to summarize dataset
# This function would extract and format the dataset's structure for printing
summarize_dataset <- function(dataset) {
  # For demonstration, return a simple summary
  # In practice, this might involve generating a summary of column names, types, etc.
  summary <- tibble::tibble(
    Column = c("id", "title"),
    Type = c("integer", "character")
  )

  return(summary)
}

#' Fetch data from a database
#'
#' @param con A database connection object.
#' @param schema The schema name in the database.
#' @param table The table name to fetch.
#'
#' @return A dplyr tbl object connected to the specified table.
tbl_from_database <- function(con, schema, table) {
  dplyr::tbl(con, DBI::Id(schema = schema, table = table))
}

#' Fetch data from Google Cloud Storage
#'
#' @param schema The schema (directory) name in Google Cloud Storage.
#' @param table The table (file) name to fetch.
#' @param anonymous Logical, whether to access GCS anonymously (default: TRUE).
#'
#' @return A dataset object from the specified GCS path.
tbl_from_gcs <- function(schema, table, anonymous = TRUE) {
  bucket_path <- stringr::str_glue("{schema}/{table}")
  bucket <- arrow::gs_bucket(bucket_path, anonymous = anonymous)
  arrow::open_dataset(bucket, format = "parquet")
}
