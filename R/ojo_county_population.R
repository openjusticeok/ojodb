#' Get population data by county for Oklahoma from the Census Bureau's American Community Survey
#'
#' Summarizes and manipulates the PEP series to return Oklahoma's population by county for desired years. Useful for calculating per capita rates of county-level data.
#'
#' @param years A numeric vector of years for which to get population data
#'
#' @export ojo_county_population
#' @return A table of Oklahoma's population by county in each year specified
#' @examples
#' \dontrun{
#' ojo_county_population(2018)
#' }
#'
ojo_county_population <- function(years) {
  rlang::check_installed(pkg = "tidycensus")

  d <- dplyr::tibble()

  for (y in years) {
    d <- tidycensus::get_estimates(
      product = "population",
      year = y,
      geography = "county",
      state = "OK"
    ) |>
      dplyr::filter(.data$variable == "POP") |>
      dplyr::mutate(year = y) |>
      dplyr::bind_rows(d)
  }

  d <- d |>
    dplyr::mutate(
      court = .data$NAME |>
        stringi::stri_replace_all_regex(" County.*", "") |>
        stringi::stri_trans_toupper() |>
        stringi::stri_replace_all_regex(" ", "")
    ) |>
    dplyr::select(.data$court, .data$year, pop = .data$value)

  return(d)
}
