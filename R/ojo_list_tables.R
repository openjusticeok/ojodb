#' List all tableson the ojodb database
#'
#' Query the Open Justice Oklahoma database for the names of all tables
#'
#' @param schema The name of the schema to query
#' @param ... Placeholder for additional arguments
#' @param .con The ojodb connection to use
#'
#' @export ojo_list_tables
#' @return data, a tibble containing the names of all tables
#' @examples
#' \dontrun{
#' ojo_list_tables()
#' ojo_list_tables("all")
#' ojo_list_tables("iic")
#' }
#'
ojo_list_tables <- function(schema = "public", ..., .con = NULL) {

  if (is.null(.con)) {
    .con <- ojo_connect()
  }

  query <- glue::glue_sql(
    "SELECT table_name FROM information_schema.tables",
    if (!schema == "all") {
      "WHERE table_schema = {schema}"
    },
    .con = .con
  )

  list_tables <- function(x) {
    query <- glue::glue_sql(
      "SELECT * FROM information_schema.tables WHERE table_schema = {x}",
      .con = .con
    )

    pool::dbGetQuery(.con, query) |>
      dplyr::as_tibble() |>
      dplyr::select(table = table_name)
  }

  if (schema == "all") {
    schemas <- ojo_list_schemas()
    data <- schemas |>
      dplyr::mutate(table = purrr::map(schema, list_tables)) |>
      tidyr::unnest(cols = table)
    return(data)
  } else {
    data <- list_tables(schema)
    data <- data |>
      dplyr::mutate(schema = schema, .before = table)
    return(data)
  }
}
