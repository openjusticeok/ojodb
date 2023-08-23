#' List all schemas on the ojodb database
#'
#' Query the Open Justice Oklahoma database for the names of all schemas
#'
#' @param ... Placeholder
#' @param .con The ojodb connection to use
#'
#' @export ojo_list_schemas
#' @return data, a tibble containing the names of all schemas
#' @examples
#' \dontrun{
#' ojo_list_schemas()
#' }
#'
ojo_list_schemas <- function(..., .con = NULL) {
  if (is.null(.con)) {
    .con <- ojo_connect()
  }

  ojo_query(
    "SELECT schema_name FROM information_schema.schemata",
    .con = .con
  ) |>
    dplyr::as_tibble() |>
    dplyr::rename(schema = schema_name) |>
    dplyr::filter(!.data$schema %in% c("pg_catalog", "information_schema")) |>
    dplyr::arrange(.data$schema)
}
