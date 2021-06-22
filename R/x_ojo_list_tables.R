#' List the tables in the \code{ojo} database
#'
#' @param pattern Filter tables that contain a string or match a regular expression
#' @return A character vector listing the tables in the \code{ojo} database that match the pattern
#' @examples
#' \dontrun{
#' ojo_list_tables()
#' ojo_list_tables("oscn")
#' }
#'

ojo_list_tables <- function(pattern = "", complete = FALSE) {

  if(!complete) {
    d <- dbListTables(ojo_db) %>%
        str_remove_all("\\d{4}.*") %>%
        unique %>%
        str_subset(pattern)
  } else {
    d <- dbListTables(ojo_db) %>%
        unique %>%
        str_subset(pattern)
  }
  
  return(d)
}
