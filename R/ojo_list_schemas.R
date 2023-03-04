#' List all schemas on the ojodb database
#'
#' Query the Open Justice Oklahoma database for the names of all schemas
#'
#' @export ojo_list_schemas
#' @return data, a tibble containing the names of all schemas
#' @examples
#' \dontrun{
#' ojo_list_schemas()
#' }
#'
ojo_list_schemas <- function(..., .con = ojo_connect()) {
  .con |>
    pool::dbGetQuery(dbplyr::sql("SELECT schema_name FROM information_schema.schemata")) |>
    dplyr::as_tibble() |>
    dplyr::rename(schema = schema_name) |>
    dplyr::filter(!schema %in% c("pg_catalog", "information_schema"))
}
