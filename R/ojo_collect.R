#' Collect data from the ojo database and close the database connection
#' @
#' @return A tibble
#' @examples
#' ojo_tbl("oscn_caseinfo") %>% 
#'     filter(court == "TULSA", casetype == "CM", file_year == 2019) %>% 
#'     ojo_collect

ojo_collect <- function(x) {
  d <- collect(x)
  
  disconnect_ojo()
  
  return(d)
}