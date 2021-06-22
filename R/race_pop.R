#' Get population data by race and sex for Oklahoma from the Census Bureau's American Community Survey
#'
#' Summarizes tables in the ACS B01001 series to return Oklahoma's population by race and sex for desired years. Useful for calculating per capita rates of incarceration.
#'
#' @param years A numeric vector of years for which to get population data
#' @return A table of Oklahoma's population by race and sex in each year specified
#' @examples
#' \dontrun{
#' race_pop(2018)
#' }

race_pop <- function(years, .incl_total = F, .race_type = "distinct") {

  d <- tibble()

  for(y in years) {
    d <- tidycensus::get_estimates(product = "characteristics",
                                   breakdown = c("RACE", "SEX", "HISP", "AGEGROUP"),
                                   breakdown_labels = T,
                                   year = y,
                                   geography = "county",
                                   state = "OK") |>
      mutate(year = y) |>
      bind_rows(d)
  }

  d <- d |>
    mutate(court = NAME |>
             str_remove(" County.*") |>
             str_to_upper() |>
             str_remove_all(" "),
           pop = value,
           race = RACE,
           sex = SEX,
           hisp = HISP,
           age = AGEGROUP) |>
    select(year, court, pop, race, sex, hisp, age)

  if(.incl_total == F) {
    d <- d |>
      filter(!race == "All races",
             !sex == "Both sexes",
             !hisp == "Both Hispanic Origins",
             !age == "All ages")
  }

  if(.race_type == "distinct") {
    d <- d |>
      filter(str_detect(race, "alone|(Two)") & !str_detect(race, "combination"))
  } else if(.race_type == "mixed") {
    d <- d |>
      filter(str_detect(race, "combination"))
  } else {
    stop('`.race_type` must be one of "distinct" or "mixed"')
  }

  return(d)
}


