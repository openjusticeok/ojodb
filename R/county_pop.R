#' Get population data by county for Oklahoma from the Census Bureau's American Community Survey
#'
#' Summarizes and manipulates the ACS B01003_001 table to return Oklahoma's population by county for desired years. Useful for calculating per capita rates of county-level data.
#'
#' @param years A numeric vector of years for which to get population data
#' @return A table of Oklahoma's population by county in each year specified
#' @examples
#' \dontrun{
#' county_pop(2018)
#' }

county_pop <- function(years) {
  d <- tibble()

  for (y in years) {
    d <- tidycensus::get_estimates(product = "population",
                                   year = y,
                                   geography = "county",
                                   state = "OK") |>
      filter(variable == "POP") |>
      mutate(year = y) |>
      bind_rows(d)
  }

  d <- d |>
    mutate(court = NAME |>
             str_remove(" County.*") |>
             str_to_upper() |>
             str_remove_all(" ")) |>
    select(court, year, pop = value)

  return(d)
}





