ojo_query_caseinfo <- function(courts, casetypes, file_years) {
  
  ci <- tibble()
  
  oscn_courts <- courts[courts %in% oscn_counties]
  
  connect_ojo()
  
  if (length(oscn_courts) > 0) {
    
    ci <- dbGetQuery(ojo_db, glue_sql(
      "SELECT court, casenum, file_date, close_date, judge
                          FROM oscn_caseinfo
                          WHERE court IN ({oscn_courts*})
                          AND casetype IN ({casetypes*})
                          AND file_year IN ({file_years*})", 
      .con = ojo_db)) %>%
      select(-contains("ci_id")) %>% 
      bind_rows(ci)
    
  }
  
  odcr_courts <- courts[courts %in% odcr_counties]
  
  if (length(odcr_courts > 0)) {
    ci <- dbGetQuery(ojo_db, glue_sql(
      "SELECT court, casenum, file_date
                          FROM odcr_caseinfo
                          WHERE court IN ({odcr_courts*})
                          AND casetype IN ({casetypes*})
                          AND file_year IN ({file_years*})", 
      .con = ojo_db)) %>%
      left_join(dbGetQuery(ojo_db, glue_sql(
        "SELECT *
                          FROM odcr_party
                          WHERE court IN ({odcr_courts*})
                          AND casetype IN ({casetypes*})
                          AND file_year IN ({file_years*})", 
        .con = ojo_db)) %>% 
          select(court, casenum, party_type, party) %>% 
          filter(party_type %in% c("Agency", "DA", "Judge", "Officer", "Attorney")) %>% 
          mutate(party_type = party_type %>% 
                   str_to_lower %>% 
                   str_replace("attorney", "def_att") %>% 
                   str_replace("da", "da_name")) %>%
          group_by(court, casenum, party_type) %>% 
          mutate(partytype = if_else(row_number() > 1,
                                     paste0(party_type, row_number()),
                                     party_type)) %>% 
          ungroup %>% 
          select(-party_type) %>% 
          pivot_wider(names_from = partytype, values_from = party)) %>% 
      bind_rows(ci)
  }
  
  disconnect_ojo()
  
  ci <- ci %>% 
    mutate_at(vars(contains("_date")), list(ymd))
  
  return(ci)
}
