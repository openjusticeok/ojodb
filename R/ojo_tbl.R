#' Identify a table from the \code{ojo} database
#'
#' Connects to the ojo database, if no connection exists, and creates a pointer to a specified table.
#'
#' @param tbl_name The name of a table in the \code{ojo} database. To get a list of tables, run \code{ojo_list_tables()}
#' @return A pointer to a table that can be passed to dplyr functions and/or pulled into a dataframe using \code{ojo_collect()}
#' @examples
#' ojo_tbl("oscn_caseinfo")
#' @seealso ojo_list_tables()

ojo_tbl <- function(tbl_name) {

  d <- tbl(ojo_db, tbl_name)

  return(d)
}
