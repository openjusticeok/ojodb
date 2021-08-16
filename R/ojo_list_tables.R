#' List all tableson the ojodb database
#'
#' Query the Open Justice Oklahoma database for the names of all tables
#'
#' @export ojo_list_tables
#' @return data, a tibble containing the names of all tables
#' @examples
#' \dontrun{
#' ojo_list_tables()
#' }
#'

ojo_list_tables <- function() {
  if(!exists("ojodb", where = .GlobalEnv)) {
    ojo_connect()
  }

  ojodb |>
    dbListTables()
}
