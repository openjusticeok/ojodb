#' Get population data by race, sex, age group, and Hispanic ethnicity for Oklahoma counties from the Census Bureau's American Community Survey
#'
#' Summarizes tables in the PEP series to return Oklahoma's population by race and sex for desired years. Useful for calculating per capita rates of incarceration.
#'
#' @param years A numeric vector of years for which to get population data
#' @param characteristics A character vector specifying which characteristics to include
#' @param .incl_total A logical value which specifies whether to include rows for the total population of certain group, e.g. all ages
#' @param .race_type A character string specifying whether to use distinct or mixed race encoding
#' @return A table of Oklahoma's population by race and sex in each year specified
#' @examples
#' \dontrun{
#' county_demographics(2018)
#' }

county_demographics <- function(years,
                                characteristics = c("race",
                                                    "sex",
                                                    "age",
                                                    "hisp"),
                                ...,
                                .incl_total = F,
                                .race_type = "distinct") {

  race_type_opts <- c("distinct", "mixed")
  characteristics_opts <- c("race", "sex", "age", "hisp")
  characteristics_labels <- c("RACE", "SEX", "AGEGROUP", "HISP")

  if(!.race_type %in% race_type_opts) {
    stop('`.race_type` must be one of "distinct" or "mixed"')
  }

  if(any(!characteristics %in% characteristics_opts) | length(characteristics) == 0) {
    stop('`characteristics` must be either a string, or a character vector containing one or more of c("sex", "age", "race", "hisp")')
  }

  ch <- tibble(labels = characteristics_labels, opts = characteristics_opts) |>
    filter(opts %in% characteristics) |>
    pull(labels)

  d <- tibble()

  for(y in years) {
    d <- tidycensus::get_estimates(product = "characteristics",
                                   breakdown = ch,
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
           pop = value)

  if("race" %in% characteristics) {
    d <- d |>
      mutate(race = RACE) |>
      select(-RACE)

    if(.race_type == "distinct") {
      d <- d |>
        filter(str_detect(race, "alone|(Two)") & !str_detect(race, "combination"))
    } else if(.race_type == "mixed") {
      d <- d |>
        filter(str_detect(race, "combination"))
    }

    if(.incl_total == F) {
      d <- d |>
        filter(!race == "All races")
    }
  }

  if("sex" %in% characteristics) {
    d <- d |>
      mutate(sex = SEX) |>
      select(-SEX)

    if(.incl_total == F) {
      d <- d |>
        filter(!sex == "Both sexes")
    }
  }

  if("age" %in% characteristics) {
    d <- d |>
      mutate(age = AGEGROUP) |>
      select(-AGEGROUP)

    if(.incl_total == F) {
      d <- d |>
        filter(!age == "All ages")
    }
  }

  if("hisp" %in% characteristics) {
    d <- d |>
      mutate(hisp = HISP) |>
      select(-HISP)

    if(.incl_total == F) {
      d <- d |>
        filter(!hisp == "Both Hispanic Origins")
    }
  }

  d <- d |>
    mutate(pop = value) |>
    select(-c(GEOID, NAME, value))

  return(d)
}


