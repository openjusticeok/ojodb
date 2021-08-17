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

ojo_list_schemas <- function() {
  if(!exists("ojodb", where = .GlobalEnv)) {
    ojo_connect()
  }

  ojodb |>
    dbGetQuery(sql("SELECT schema_name FROM information_schema.schemata")) |>
    as_tibble() |>
    rename(schema = schema_name) |>
    filter(!schema %in% c("pg_catalog", "information_schema"))
}
