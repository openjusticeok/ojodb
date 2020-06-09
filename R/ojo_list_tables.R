#' List the tables in the \code{ojo} database  
#'
#' @param pattern Filter tables that contain a string or match a regular expression
#' @return A character vector listing the tables in the \code{ojo} database that match the pattern
#' @examples
#' ojo_list_tables()
#' ojo_list_tables("oscn")

ojo_list_tables <- function(pattern = "") {
  connect_ojo()
  
  d <- dbListTables(ojo_db) %>% 
    str_remove_all("\\d{4}.*") %>% 
    unique %>% 
    str_subset(pattern)
  
  disconnect_ojo()
  
  return(d)
}
