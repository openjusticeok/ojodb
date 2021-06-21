#' List the variables in an \code{ojo} database table
#'
#' @param table Name of the table as a string, e.g. "cases"
#' @return A character vector listing the variables in the \code{ojo} database table specified
#' @examples
#' \dontrun{
#' ojo_list_vars("case")
#' }

ojo_list_vars <- function(table) {
  ojodb <- ojo_connect()

  d <- dbListFields(ojodb, table)

  return(d)
}
