#' Identify a table from the OJO database
#'
#' Identifies a table in the OJO database from which to query data. Remember to run \code{connect_ojo()} to establish a connection before attempting to query and to close the connection afterwards with \code{disconnect_ojo()}.
#'
#' @aliases ojo_tbl
#'
#' @param table The name of a table in the OJO database. To get a list of tables, run \code{ojo_list_tables()}
#' @param schema The name of a schema in the OJO database. To get a list of schemas, run \code{ojo_list_schemas()}
#' @param ... Placeholder
#' @param .con The ojodb connection to use
#'
#' @export ojo_tbl
#' @return A pointer to a table that can be passed to dplyr functions and/or pulled into a dataframe using \code{ojo_collect()}
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
    .source = "database",
    .cache = rlang::is_interactive()
) {
  if (is.null(.con)) {
    .con <- ojo_connect(...)
  }

  if (.source == "database") {
    return(tbl_from_database(.con, table, schema))
  } else if (.source == "gcs") {
    # Temp fix for schema
    if (schema == "public") {
      schema <- "oscn"
    }

    if (.cache) {

    } else {
      return(tbl_from_gcs(schema, table))
    }
  } else {
    rlang::abort("Invalid source specified. Please choose one of: 'database' or 'gcs'.")
  }
}

tbl_from_database <- function(con, table, schema) {
  data_tbl <- dplyr::tbl(
    con,
    DBI::Id(
      schema = schema,
      table = table
    )
  )

  return(data_tbl)
}

tbl_from_gcs <- function(schema, table) {
  bucket <- arrow::gs_bucket(
    stringr::str_glue("{schema}/{table}"),
    anonymous = TRUE
  )

  data_tbl <- arrow::open_dataset(bucket, format = "parquet")

  return(data_tbl)
}
