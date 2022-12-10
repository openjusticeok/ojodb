#' Query minutes for a given search term
#'
#' Query the Open Justice Oklahoma database for minutes matching a search term
#' 
#' @param query A term or phrase to search for using the PostgreSQL full text search syntax
#'
#' @export ojo_search_minutes
#' @return data, a lazy tibble containing the resulting minutes
#' @examples
#' \dontrun{
#'
#' # Minutes containing words like 'mcgirt'
#' ojo_search_minutes("mcgirt")
#'
#' # Minutes containing 'mcgirt' AND 'jursdiction
#' ojo_search_minutes("mcgirt & jurisdiction")
#'
#' # Minutes containing 'mcgirt' OR 'jurisdiction
#' ojo_search_minutes("mcgirt | jurisdiction")
#'
#' # Minutes NOT containing 'mcgirt'
#' ## Use sparingly! It is likely to return rows in the hundreds of thousands or millions!
#' ojo_search_minutes("!! mcgirt")
#'
#' # Minutes containing 'tribal' FOLLOWED BY 'jurisdiction'
#' ojo_search_minutes("tribal <-> jurisdiction")
#' }
#'
ojo_search_minutes <- function(query) {
  ojo_connect()
  q <- glue::glue_sql("SELECT * FROM minute WHERE to_tsvector('english', description) @@ to_tsquery('english', {query});",
    .con = ojodb
  )
  pool::dbGetQuery(ojodb, q) |>
    dplyr::as_tibble()
}
