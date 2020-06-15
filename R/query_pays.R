ojo_query_pays <- function(courts, casetypes, file_years, pay_years = NA) {
  
  pay_yr_query <- if_else(all(is.na(pay_years)),
                          "",
                          paste0(" AND pay_date >= '", min(pay_years), "-01-01'",
                                " AND pay_date <= '", max(pay_years), "-12-31'"))
  
  pays <- tibble()
  
  oscn_courts <- courts[courts %in% oscn_counties]
  odcr_courts <- courts[courts %in% odcr_counties]
  
  connect_ojo()
  
  if (length(oscn_courts) > 0) {
    for (y in file_years) {
      tbl_name <- paste0("oscn_pays_", 
                         y)
      
      pays <- dbGetQuery(ojo_db, 
                         glue_sql(
                           paste0(
                             "SELECT *
                              FROM {`tbl_name`}
                              WHERE court IN ({oscn_courts*})
                              AND casetype IN ({casetypes*})",
                             pay_yr_query),
                           .con = ojo_db
                         )) %>% 
        select(-oscn_pay_id) %>% 
        bind_rows(pays)
      
      print(paste(y, "payments queried for OSCN courts:", paste(oscn_courts, collapse = ", ")))
      
    }
  }
  
  if (length(odcr_courts) > 0) {
    for (y in file_years) {
      tbl_name <- paste0("odcr_pays_", 
                         y)
      
      pays <- dbGetQuery(ojo_db, glue_sql(
        paste0(
        "SELECT *
            FROM {`tbl_name`}
            WHERE court IN ({odcr_courts*})
          AND casetype IN ({casetypes*})",
        pay_yr_query),
        .con = ojo_db
      )) %>% 
        select(-odcr_pay_id) %>% 
        bind_rows(pays)
      
      print(paste(y, "payments queried for ODCR courts:", paste(odcr_courts, collapse = ", ")))
      
    }
  }
  
  pays <- pays %>% 
    mutate_at(vars(contains("_date")), list(ymd))
  
  disconnect_ojo()
  
  return(pays)
  
}
