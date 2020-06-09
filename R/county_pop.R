county_pop <- function(years) {
  d <- tibble()

  for (y in years) {
    d <- tidycensus::get_acs("county",
                 variables = c(pop = "B01003_001"),
                 state = "OK",
                 year = y) %>%
      mutate(year = y) %>%
      bind_rows(d)
  }

  d <- d %>%
    mutate(court = NAME %>%
             str_remove(" County.*") %>%
             str_to_upper %>%
             str_remove_all(" ")) %>%
    select(court, year, pop = estimate)

  return(d)
}





