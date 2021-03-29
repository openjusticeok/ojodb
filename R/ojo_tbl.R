#' Identify a table from the OJO database
#'
#' Identifies a table in the OJO database from which to query data. Remember to run \code{connect_ojo()} to establish a connection before attempting to query and to close the connection afterwards with \code{disconnect_ojo()}.
#'
#' @aliases ojo_tbl ojo_table
#' @export ojo_tbl ojo_table
#' @param tbl_name The name of a table in the OJO database. To get a list of tables, run \code{ojo_list_tables()}
#' @return A pointer to a table that can be passed to dplyr functions and/or pulled into a dataframe using \code{ojo_collect()}
#' @examples
#' \dontrun{
#' # Identifies the table
#' ojo_tbl("oscn_caseinfo")
#'
#' # Pulls down case information data for every Tulsa felony filed in 2020 into a dataframe d
#' d <- ojo_tbl("oscn_caseinfo") %>%
#'     filter(court == "TULSA", casetype == "CF", file_year == 2020) %>%
#'     collect()
#'}
#' @seealso ojo_list_tables(), ojo_list_vars()

ojo_tbl <- function(tbl_name) {

  tbl(ojo_db, tbl_name)

}

ojo_table <- ojo_tbl
