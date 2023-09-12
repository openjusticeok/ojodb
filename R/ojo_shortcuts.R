#' Returns a list of the OSCN-reporting counties.
#'
#' Quickly retrieve a list of all the counties that report data on OSCN, in either all-upper, all-lower, or title case.
#'
#' @export list_oscn_counties
#' @returns A tibble with a list of the counties that report on OSCN
#'
#' @examples
#' \dontrun{
#' list_oscn_counties()
#' }
#'

list_oscn_counties <- function(case = "upper"){

  if(!case %in% c("upper", "lower", "title")){
    stop("Case must be either 'upper', 'lower', or 'title'.")
  }

  list <- c("ADAIR", "CANADIAN", "CLEVELAND", "COMANCHE",
      "ELLIS", "GARFIELD", "LOGAN", "OKLAHOMA", "PAYNE",
      "PUSHMATAHA", "ROGER MILLS", "ROGERS", "TULSA")

  if(case == "lower") {
    return(tolower(list))
  } else if (case == "title") {
    return(janitor::make_clean_names(list, case = "title"))
  } else {
    return(list)
  }

}
