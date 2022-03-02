#' List all tableson the ojodb database
#'
#' Query the Open Justice Oklahoma database for the names of all tables
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
  if(!exists("ojodb", where = .GlobalEnv)) {
    ojo_connect()
  }

  list_tables <- function(x) {
    query <- sqlInterpolate(ojodb, "SELECT * FROM information_schema.tables WHERE table_schema = ?schema",
                            schema = x)

    dbGetQuery(ojodb, query) |>
      as_tibble() |>
      select(table = table_name)
  }

  if(schema == "all") {
    schemas <- ojo_list_schemas()
    data <- schemas |>
      mutate(table = map(schema, list_tables)) |>
      unnest(cols = table)
    return(data)
  } else {
    data <- list_tables(schema)
    data <- data |>
      mutate(schema = schema, .before = table)
    return(data)
  }
}
