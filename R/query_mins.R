ojo_query_mins <- function(courts, casetypes, file_years, min_years = NA, min_type = "all", min_codes = NULL) {
  
  min_yr_query <- if_else(all(is.na(min_years)),
                          "",
                          paste0(" AND min_date >= '", min(min_years), "-01-01'",
                                 " AND min_date <= '", max(min_years), "-12-31'"))
  
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
          codes_query,
          min_yr_query),
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
        
        mins <- dbGetQuery(ojo_db, 
                           glue_sql(
                             paste0(
                               "SELECT *
                                FROM {`tbl_name`}
                                WHERE court IN ({odcr_courts*})",
                               min_yr_query),
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
