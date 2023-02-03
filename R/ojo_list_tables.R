#' List all tableson the ojodb database
#'
#' Query the Open Justice Oklahoma database for the names of all tables
#'
#' @param schema The name of the schema to query
#' @param ... Placeholder for additional arguments
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
ojo_list_tables <- function(schema = "public", ...) {
  if (!exists("ojodb", where = .GlobalEnv)) {
    ojo_connect()
  }

  list_tables <- function(x) {
    query <- DBI::sqlInterpolate(
      ojodb,
      "SELECT * FROM information_schema.tables WHERE table_schema = ?schema",
      schema = x
    )

    pool::dbGetQuery(ojodb, query) |>
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
