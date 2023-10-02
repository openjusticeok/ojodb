#' @import S7
class_ojo_tbl <- S7::new_class(
  "ojo_tbl",
  properties = list(
    con = S7::class_any
  )
)

class_ojo_postgres_tbl <- S7::new_class(
  "ojo_postgres_tbl",
  parent = class_ojo_tbl,
  constructor = function(.data) {
    S7::new_object(.data)
  }
)

class_ojo_arrow_tbl <- S7::new_class(
  "ojo_arrow_tbl",
  parent = class_ojo_tbl,
  constructor = function(.data) {
    S7::new_object(.data)
  }
)

new_ojo_postgres_tbl <- function(...) {
  con <- ojo_connect()
  parent_instance <- class_ojo_tbl(con = con)

  class_ojo_postgres_tbl(parent_instance)
}

new_ojo_arrow_tbl <- function(...) {
  parent_instance <- class_ojo_tbl(con = NULL)
  class_ojo_arrow_tbl(parent_instance)
}

new_ojo_tbl <- function(source, ...) {
  switch(
    source,
    postgres = new_ojo_postgres_tbl(...),
    arrow = new_ojo_arrow_tbl(...)
  )
}

get_data <- S7::new_generic("get_data", "x")

S7::method(get_data, class_ojo_postgres_tbl) <- function(x, schema, table,...) {
  tbl(x@con, DBI::Id(table = table, schema = schema))
}

S7::method(get_data, class_ojo_arrow_tbl) <- function(x, schema, table, ...) {
  if (schema == "public") {
    rlang::warn(
      "Schema `public` should no longer be used, and will be result in an error in future package versions. Automatically using schema `oscn` instead. This message won't be shown again this session.",
      .frequency = "once",
      .frequency_id = "ojo_schema_public_warning"
    )
    schema <- "oscn"
  }
  path <- glue::glue("{schema}/{table}")
  bucket <- arrow::gs_bucket(bucket = path)
  arrow::open_dataset(bucket)
}

#' Identify a table from the OJO database
#'
#' Identifies a table in the OJO database from which to query data. Remember to run \code{connect_ojo()} to establish a connection before attempting to query and to close the connection afterwards with \code{disconnect_ojo()}.
#'
#' @aliases ojo_tbl
#'
#' @param table The name of a table in the OJO database. To get a list of tables, run \code{ojo_list_tables()}
#' @param schema The name of a schema in the OJO database. To get a list of schemas, run \code{ojo_list_schemas()}
#' @param source The source of the data. Defaults to `postgres`. Alternative values include `arrow` for snappier queries on older data
#' @param ... Placeholder
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
ojo_tbl <- function(table, schema = "public", source = "postgres", ...) {
  obj <- new_ojo_tbl(source, ...)
  get_data(obj, schema, table)
}
