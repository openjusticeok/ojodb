#' List all variables in a table on the ojodb database
#'
#' Query the Open Justice Oklahoma database for the names of all variables in a table
#'
#' @param table The name of the table to query
#' @param schema The name of the schema to query
#' @param ... Placeholder for additional arguments
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
ojo_list_vars <- function(table, schema = "public", ...) {

  if (!exists("ojodb", where = .GlobalEnv)) {
    ojo_connect()
  }

  query <- glue::glue_sql(
    "SELECT column_name FROM information_schema.columns WHERE table_schema = {schema} AND table_name = {table}",
    .con = ojodb
  )

  ojodb |>
    pool::dbGetQuery(query)

}
