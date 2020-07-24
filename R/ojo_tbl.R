#' Identify a table from the OJO database
#'
#' Identifies a table in the OJO database from which to query data. Remember to run \code{connect_ojo()} to establish a connection before attempting to query and to close the connection afterwards with \code{disconnect_ojo()}.
#'
#' @param tbl_name The name of a table in the OJO database. To get a list of tables, run \code{ojo_list_tables()}
#' @return A pointer to a table that can be passed to dplyr functions and/or pulled into a dataframe using \code{ojo_collect()}
#' @examples
#' ojo_tbl("oscn_caseinfo")
#' @seealso ojo_list_tables(), ojo_list_vars()

ojo_tbl <- function(tbl_name) {

  tbl(ojo_db, tbl_name)

}
