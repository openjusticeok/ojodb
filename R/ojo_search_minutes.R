#' Query minutes for a given search term
#'
#' Query the Open Justice Oklahoma database for minutes matching a search term
#'
#' @export ojo_search_minutes
#' @return data, a lazy tibble containing the resulting minutes
#' @examples
#' \dontrun{
#' ojo_search_minutes("mcgirt")
#' }
#'

ojo_search_minutes <- function(query) {
  ojo_connect()
  q <- glue_sql("SELECT * FROM minute WHERE to_tsvector('english', description) @@ to_tsquery('english', {query});",
                .con = ojodb)
  dbGetQuery(ojodb, q) |>
    as_tibble()
}
