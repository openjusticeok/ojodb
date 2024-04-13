#' @title OJO Query
#'
#' @description Query the Open Justice Oklahoma database
#'
#' @param ... Arguments to pass to glue::glue_sql
#' @param .con The ojodb connection to use
#' @param query The query to send to ojodb
#'
#' @export ojo_query
#' @returns data, a tibble containing the results of the query
#' @examples
#' \dontrun{
#' ojo_query("SELECT * FROM \"case\" LIMIT 10")
#' ojo_query("SELECT * FROM iic.inmate LIMIT 10")
#' }
#'
ojo_query <- function(query, ..., .con = NULL) {
  if (is.null(.con)) {
    .con <- ojo_connect(...)
  }

  .con |>
    pool::dbGetQuery(query) |>
    dplyr::as_tibble()
}
