#' Get representation and amount due data for Tulsa County small claims evictions cases
#'
#' Some key pieces of information about eviction cases are available in the minutes or attorneys tables and take some digging. This function takes data from the ojo database and summarizes it to the case level to allow consistent definitions and avoid having to reconstruct the measures for each project.
#'
#' @return A dataframe
#' @param file_year The year to retrieve eviction details
#' @examples
#' \dontrun{
#' d <- ojo_eviction_details(2020)
#' }

ojo_eviction_details <- function(file_year) {
  connect_ojo()

  eviction_cases <- ojo_tbl("ojo_civ_cases") %>%
    filter(court == "TULSA",
           casetype == "SC",
           issue == "EVICTION",
           file_year == file_year) %>%
    collect()

  # Querying for minutes with the minute code "A/"
  debt <- ojo_tbl(paste0("oscn_mins_", file_year, "SC")) %>%
    filter(min_code == "A/",
           court == "TULSA",
           casenum %in% !!eviction_cases$casenum) %>%
    collect()

  # Extract the number from the min_desc column
  d2 <- debt %>%
    select(court, casenum, min_desc) %>%
    distinct %>%
    mutate(min = str_remove_all(min_desc, "AMOUNT IN DEBT OF |\\$|,") %>%
             str_remove_all("( PER |Document)(.|\n)*") %>%
             str_squish()) %>%
    mutate(fees = str_extract_all(min, "(\\d|\\.)+") %>%
             as.character() %>%
             str_remove_all('c\\("|"\\)')) %>%
    separate(fees,
             into = paste0("fee", 1:4),
             sep = '", "') %>%
    rowwise(court, casenum, min_desc) %>%
    mutate(across(contains("fee"), as.numeric),
           debt_amt = sum(fee1, fee2, fee3, fee4, na.rm = TRUE),
           late_fee = str_detect(min_desc, "LATE")) %>% # Add variable for late fee
    group_by(court, casenum) %>%
    summarize(debt_amt = sum(debt_amt),
              late_fee = any(late_fee == TRUE))

  a <- ojo_tbl("oscn_atts") %>%
    filter(court == "TULSA",
           casetype == "SC",
           file_year == file_year,
           casenum %in% !!eviction_cases$casenum) %>%
    mutate(rep_party = str_to_upper(rep_party)) %>%
    left_join(ojo_tbl("oscn_parties") %>%
                rename(rep_party = party_name)) %>%
    collect() %>%
    mutate(party_type = if_else(is.na(party_type),
                                case_when(str_detect(att_name, "MILNER|PERSONS|DECARLO|BLAINE|KNOPP|DRYER|HUDDLESTON|GIBBS|ZANNOTTI") ~ "Plaintiff",
                                          str_detect(rep_party, "LLC") ~ "Plaintiff",
                                          str_detect(att_address, "907") & str_detect(att_address, "DETROIT") ~ "Defendant"),
                                party_type))

  a_case <- a %>%
    filter(!is.na(party_type)) %>%
    group_by(court, casenum) %>%
    summarize(rep_def = any(party_type == "Defendant"),
              att_def = if_else(rep_def == TRUE,
                                first(att_name[which(party_type == "Defendant")]),
                                as.character(NA)),
              rep_plaint = any(party_type == "Plaintiff"),
              att_plaint = if_else(rep_plaint == TRUE,
                                   first(att_name[which(party_type == "Plaintiff")]),
                                   as.character(NA)))

  disp <- ojo_tbl(paste0("oscn_mins_", file_year, "SC")) %>%
    filter(str_detect(min_code, "DISP"),
           court == "TULSA",
           casenum %in% !!eviction_cases$casenum) %>%
    select(-oscn_min_id) %>%
    distinct() %>%
    collect()

  disconnect_ojo()

  dat <- d2 %>%
    left_join(a_case) %>%
    mutate(across(starts_with("rep"), replace_na, replace = FALSE))

  return(dat)
}

