#' Identify a table from the OJO database
#'
#' Identifies a table in the OJO database from which to query data. Remember to run \code{connect_ojo()} to establish a connection before attempting to query and to close the connection afterwards with \code{disconnect_ojo()}.
#'
#' @aliases ojo_tbl ojo_table
#' @export ojo_tbl ojo_table
#' @param table The name of a table in the OJO database. To get a list of tables, run \code{ojo_list_tables()}
#' @param schema The name of a schema in the OJO database. To get a list of schemas, run \code{ojo_list_schemas()}
#'
#' @return A pointer to a table that can be passed to dplyr functions and/or pulled into a dataframe using \code{ojo_collect()}
#' @section Aliases:
#'  For comfort, `ojo_tbl` and `ojo_table` can be used interchangeably.
#' @examples
#' \dontrun{
#' # Identifies the table
#' ojo_tbl("case")
#'
#' # Pulls down case information data for every Tulsa felony filed in 2020 into a dataframe d
#' d <- ojo_tbl("case") %>%
#'     filter(district == "TULSA", case_type == "CF", year == 2020) %>%
#'     collect()
#'}
#' @seealso ojo_list_tables(), ojo_list_vars(), ojo_list_schemas()
#'
ojo_tbl <- function(table, schema = "public") {

  if(!exists("ojodb", where = .GlobalEnv)) {
    ojo_connect()
  }

  pool_src <- poolCheckout(ojodb)
  on.exit(poolReturn(pool_src))
  pool_src |>
    tbl(in_schema(schema, table))
}

ojo_table <- ojo_tbl
