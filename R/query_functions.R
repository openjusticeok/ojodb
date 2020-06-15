ojo_query_disps <- function(courts, casetypes, file_years) {
  
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
  
  return(disps)
  
}

ojo_query_mins <- function(courts, casetypes, file_years, min_type = "all", min_codes = NULL) {
  
  mins <- tibble()
  
  oscn_courts <- courts[courts %in% oscn_counties]
  odcr_courts <- courts[courts %in% odcr_counties]
  
  connect_ojo()
  
  if (length(oscn_courts) > 0) {
    
    codes_query <- case_when(!is.null(min_codes) ~ " AND min_code IN ({min_codes*})",
                             min_type == "all" ~ "",
                             min_type == "fees" ~ " AND fee_amt IS NOT NULL")
    
    for (t in casetypes) {
      for (y in file_years) {
        tbl_name <- paste0("oscn_mins_", 
                           y, t)
        
        mins <- dbGetQuery(ojo_db, glue_sql(paste0(
          "SELECT *
            FROM {`tbl_name`}
            WHERE court IN ({oscn_courts*})",
          codes_query),
          .con = ojo_db
        )) %>% 
          select(-oscn_min_id, -min_row) %>% 
          bind_rows(mins)
        
        print(paste(t, y, "minutes queried for OSCN courts:", paste(oscn_courts, collapse = ", ")))
        
      }
    }
  }
  
  if (length(odcr_courts) > 0) {
    for (t in casetypes) {
      for (y in file_years) {
        tbl_name <- paste0("odcr_mins_", 
                           y, t)
        
        mins <- dbGetQuery(ojo_db, glue_sql(
          "SELECT *
            FROM {`tbl_name`}
            WHERE court IN ({odcr_courts*})",
          .con = ojo_db
        )) %>% 
          select(-odcr_min_id) %>% 
          bind_rows(mins)
        
        print(paste(t, y, "minutes queried for ODCR courts:", paste(odcr_courts, collapse = ", ")))
        
      }
    }
  }
  
  if (min_type == "fees") {
    mins <- mins %>% 
      filter(fee_amt != 0)
  }
  
  mins <- mins %>% 
    mutate_at(vars(contains("_date")), list(ymd))
  
  disconnect_ojo()
  
  return(mins)
  
}

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
      "SELECT court, casenum, file_date, judge
                          FROM odcr_caseinfo
                          WHERE court IN ({odcr_courts*})
                          AND casetype IN ({casetypes*})
                          AND file_year IN ({file_years*})", 
      .con = ojo_db)) %>% 
      select(-odcr_ci_id) %>% 
      bind_rows(ci)
  }
  
  disconnect_ojo()
  
  ci <- ci %>% 
    mutate_at(vars(contains("_date")), list(ymd))
  
  return(ci)
}
