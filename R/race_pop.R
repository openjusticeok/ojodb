race_pop <- function(years) {

  data <- tibble()

  state_race <- tibble()

  for (y in years) {
    for (l in LETTERS[1:9]) {
      state_race <- get_acs("county",
                            year = y,
                            table = paste0("B01001", l),
                            state = "OK",
                            survey = "acs5",
                            cache_table = TRUE) %>%
        bind_rows(state_race)
    }

    message("Data for ", y, " returned.")

    race_sum <- state_race %>%
      mutate(tno = str_sub(variable, 9, 11) %>%
               as.numeric,
             sex = case_when(tno >= 7 & tno <= 16 ~ "M",
                             tno >= 22 & tno <= 31 ~ "F",
                             TRUE ~ as.character(NA)),
             race = case_when(str_detect(variable, "1B") ~ "BLACK",
                              str_detect(variable, "1C") ~ "NATIVE AMERICAN",
                              str_detect(variable, "1D") ~ "ASIAN",
                              str_detect(variable, "1E") ~ "PACIFIC ISLANDER",
                              str_detect(variable, "1H") ~ "WHITE",
                              str_detect(variable, "1I") ~ "HISPANIC",

                              TRUE ~ as.character(NA)
             )) %>%
      filter(!is.na(sex), !is.na(race)) %>%
      group_by(sex, race) %>%
      summarize(pop = sum(estimate)) %>%
      mutate(year = y)

    data <- bind_rows(data, race_sum)
  }
  return(data)
}
