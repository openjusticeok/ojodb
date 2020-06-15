ojo_query_disps <- function(courts, casetypes, file_years, caseinfo = TRUE) {
  
  disps <- tibble()
  
  connect_ojo()
  
  oscn_courts <- courts[courts %in% oscn_counties]
  
  if (length(oscn_courts) > 0) {
    oscn_tbl <- ifelse(any(c("CM", "CF", "TR") %in% casetypes), 
                       "oscn_crim_disps",
                       "oscn_civ_disps")
    
    disps <- dbGetQuery(ojo_db, glue_sql(
      "SELECT *
                          FROM {`oscn_tbl`}
                          WHERE court IN ({oscn_courts*})
                          AND casetype IN ({casetypes*})
                          AND file_year IN ({file_years*})", 
      .con = ojo_db)) %>%
      select(-contains("disp_id")) %>%
      bind_rows(disps)
  }
  
  odcr_courts <- courts[courts %in% odcr_counties]
  
  if (length(odcr_courts > 0)) {
    disps <- dbGetQuery(ojo_db, glue_sql(
      "SELECT *
                          FROM odcr_disps
                          WHERE court IN ({odcr_courts*})
                          AND casetype IN ({casetypes*})
                          AND file_year IN ({file_years*})", 
      .con = ojo_db)) %>% 
      filter(ct_desc != "PAY ONLINE", ct_desc != "DISMISSED") %>% 
      left_join(dbGetQuery(ojo_db, glue_sql(
        "SELECT *
                          FROM odcr_party
                          WHERE court IN ({courts*})
                          AND casetype IN ({casetypes*})
                          AND file_year IN ({file_years*})
                          AND party_type = 'Defendant'", 
        .con = ojo_db)) %>% 
          select(-party_type, -party_loc, -odcr_pn_id) %>% 
          rename(defname = party)) %>% 
      select(-odcr_disp_id) %>% 
      group_by(casenum, ct_desc, defname) %>%
      mutate(ct_no = ifelse(ct_no > min(ct_no), ct_no - 1, ct_no)) %>% 
      distinct %>% 
      bind_rows(disps)
  }
  
  disconnect_ojo()
  
  disps <- disps %>% 
    mutate_at(vars(contains("_date")), list(ymd))
  
  if (caseinfo == TRUE) {
    disps <- disps %>% 
      left_join(ojo_query_caseinfo(courts, casetypes, file_years))
  }
  
  return(disps)
  
}


ojo_query_offenses <- function(courts, casetypes, file_years, offense_string, include_caseinfo = TRUE) {
  d <- tibble()
  
  for (i in courts) {
    d <- d %>% 
      bind_rows(ojo_query_disps(i, casetypes, file_years)) %>% 
      group_by(court, casenum, defname) %>% 
      filter(any(str_detect(ct_desc, offense_string)))
    
    print(paste0("Charges filed in ", i, " queried."))
  }
  
  return(d)
  
}


