#' @title OJO Query
#'
#' @description Query the Open Justice Oklahoma database
#'
#' @param ... Arguments to pass to glue::glue_sql
#' @param .con
#'
#' @export ojo_query
#' @returns data, a tibble containing the results of the query
#' @examples
#' \dontrun{
#' ojo_query("SELECT * FROM "case" LIMIT 10")
#' ojo_query("SELECT * FROM iic.inmate LIMIT 10")
#' }
#'
ojo_query <- function(query, ..., .con = NULL) {
  if (!is.null(.con)) {
    ojo_pool <- .con
  } else {
    ojo_connect(..., .env = parent.frame())
  }

  if (!exists("ojo_pool")) {
    stop("No connection to OJO database. Please provide a database connection created with `ojo_connect` to the `.con` argument.")
  }

  pool::dbGetQuery(ojo_pool, query) |>
    dplyr::as_tibble()
}
