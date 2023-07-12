#' List all variables in a table on the ojodb database
#'
#' Query the Open Justice Oklahoma database for the names of all variables in a table
#'
#' @param table The name of the table to query
#' @param schema The name of the schema to query
#' @param ... Placeholder for additional arguments
#' @param .con The ojodb connection to use
#'
#' @export ojo_list_vars
#'
#' @return data, a tibble containing the names of all variables in a table
#'
#' @examples
#' \dontrun{
#' ojo_list_vars("case")
#' ojo_list_vars("inmate", "iic")
#' }
#'
ojo_list_vars <- function(table, schema = "public", ..., .con = NULL) {
  ojo_tbl(
    table = "columns",
    schema = "information_schema",
    .con = .con
  ) |>
    dplyr::filter(
      table_schema == schema,
      table_name == table
    ) |>
    dplyr::select(column_name) |>
    dplyr::arrange(column_name)
}
