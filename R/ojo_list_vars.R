#' List all variables in a table on the ojodb database
#'
#' Query the Open Justice Oklahoma database for the names of all variables in a table
#'
#' @export ojo_list_vars
#' @return data, a tibble containing the names of all variables in a table
#' @examples
#' \dontrun{
#' ojo_list_vars("case")
#' }
#'

ojo_list_vars <- function(table) {
  if(!exists("ojodb", where = .GlobalEnv)) {
    ojo_connect()
  }

  ojodb |>
    dbListFields(name = SQL(table))
}

