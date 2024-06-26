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
#' @param .source `r lifecycle::badge("experimental")` The source of the table. Options are 'postgres', 'gcs_duckdb', and 'gcs_arrow'. Default is 'postgres'.
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
  .source = "postgres"
) {
  if (.source == "database") {
    lifecycle::deprecate_warn("2.9.0", I(".source = 'database' is deprecated. Use '.source = 'postgres' instead."))
    .source <- "postgres"
  }

  data <- switch(
    .source,
    "postgres" = {
      if (is.null(.con)) {
        .con <- ojo_connect(
          ...,
          .driver = "RPostgres"
        )
      }
      tbl_from_rpostgres(.con, schema, table)
    },
    "gcs_duckdb" = {
      if (!rlang::is_installed("duckdb")) {
        rlang::abort(".source == \"gcs_duckdb\" requires {duckdb}.")
      }
      if (is.null(.con)) {
        .con <- ojo_connect(..., .driver = "duckdb", .source = .source)
      }
      if (schema == "public") {
        schema <- "oscn"
      }
      tbl_from_gcs_duckdb(.con, schema, table)
    },
    "gcs_arrow" = {
      if (!rlang::is_installed("arrow")) {
        rlang::abort(".source == \"gcs_arrow\" requires {arrow} with GCS support.")
      }
      gcs_available <- arrow::arrow_with_gcs()
      if (!gcs_available) {
        rlang::abort("Arrow wasn't compiled with GCS support.")
      }
      if (schema == "public") {
        schema <- "oscn"
      }
      tbl_from_gcs_arrow(schema, table)
    },
    rlang::abort("Invalid source specified. Please choose one of: 'postgres', 'gcs_duckdb', or 'gcs_arrow'.")
  )

  class(data) <- c("ojo_tbl", class(data))

  return(data)
}

#' Fetch data from a database
#'
#' @param con A database connection object.
#' @param schema The schema name in the database.
#' @param table The table name to fetch.
#'
#' @return A dplyr tbl object connected to the specified table.
#'
#' @keywords internal
#'
tbl_from_rpostgres <- function(con, schema, table) {
  dplyr::tbl(con, DBI::Id(schema = schema, table = table))
}

#' Fetch data from Google Cloud Storage
#'
#' @param schema The schema (directory) name in Google Cloud Storage.
#' @param table The table (file) name to fetch.
#' @param anonymous Logical, whether to access GCS anonymously (default: TRUE).
#'
#' @return A dataset object from the specified GCS path.
#'
#' @keywords internal
#'
tbl_from_gcs_arrow <- function(schema, table, anonymous = TRUE) {
  bucket_path <- stringr::str_glue("{schema}/{table}")
  bucket <- arrow::gs_bucket(bucket_path, anonymous = anonymous)
  arrow::open_dataset(bucket, format = "parquet")
}

#' Fetch data from Google Cloud Storage using {duckdb}
#'
#' @param schema The schema (directory) name in Google Cloud Storage.
#' @param table The table (file) name to fetch.
#'
#' @return A lazy {duckdb} result
#'
#' @keywords internal
#'
tbl_from_gcs_duckdb <- function(con, schema, table) {
  bucket_path <- stringr::str_glue("gs://ojo-data-warehouse/raw-data/{schema}/{table}/*.parquet")
  DBI::dbExecute(con, stringr::str_glue("CREATE OR REPLACE VIEW \'{table}\' AS SELECT * FROM read_parquet('{bucket_path}')"))
  tbl(con, table)
}
