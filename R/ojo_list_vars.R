#' List all variables in a table on the ojodb database
#'
#' Query the Open Justice Oklahoma database for the names of all variables in a table
#'
#' @export ojo_list_vars
#' @return data, a tibble containing the names of all variables in a table
#' @examples
#' \dontrun{
#' ojo_list_vars("case")
#' ojo_list_vars("inmate", "iic")
#' }
#'

ojo_list_vars <- function(table, schema = "public", ...) {
  if(!exists("ojodb", where = .GlobalEnv)) {
    ojo_connect()
  }

  query <- str_c(schema, ".", table)

  ojodb |>
    dbListFields(name = SQL(query))
}

