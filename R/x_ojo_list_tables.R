#' List the tables in the \code{ojo} database
#'
#' @return A character vector listing the tables in the \code{ojo} database
#' @examples
#' \dontrun{
#' ojo_list_tables()
#' }
#'

ojo_list_tables <- function() {
  ojodb <- ojo_connect()

  d <- dbListTables(ojodb)

  return(d)
}
