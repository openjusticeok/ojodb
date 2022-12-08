#' Determine the Oklahoma state fiscal year that a Date falls in
#'
#' Returns the Oklahoma state fiscal year (July 1 - June 30) in which a given Date falls.
#'
#' @export ojo_fiscal_year
#' @param date An atomic value of class Date
#' @return Fiscal year of a Date as an integer
#' @examples
#' \dontrun{
#' ojo_fiscal_year(ymd("2018-06-30"))
#' # Returns 2018
#'
#' ojo_fiscal_year(ymd("2018-07-01"))
#' # Returns 2019
#' }
#'
ojo_fiscal_year <- function(date) {
  dplyr::if_else(
    lubridate::month(date) > 6,
    lubridate::year(date) + 1,
    lubridate::year(date)
  )
}
