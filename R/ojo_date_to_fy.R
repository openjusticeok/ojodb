#' Determine the Oklahoma state fiscal year that a date falls in
#'
#' Returns the Oklahoma state fiscal year (July 1 - June 30) in which a given date falls.
#'
#' @return Fiscal year of a date as an integer
#' @examples
#' \dontrun{
#' ojo_date_to_fy(ymd("2018-06-30"))
#' # Returns 2018
#'
#' ojo_date_to_fy(ymd("2018-07-01"))
#' # Returns 2019
#'}

ojo_date_to_fy <- function(date) {
  ifelse(month(date) > 6, year(date) + 1, year(date))
}
