#' List the variables in an \code{ojo} database table
#'
#' @param table Name of the table as a string, e.g. "ojo_crim_cases"
#' @return A character vector listing the tables in the \code{ojo} database that match the pattern
#' @examples
#' \dontrun{
#' ojo_list_vars("oscn_civ_disps")
#' }

ojo_list_vars <- function(table) {

  d <- dbListFields(ojo_db, table)

  return(d)
}
