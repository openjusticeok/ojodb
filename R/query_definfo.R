ojo_query_definfo <- function(courts, casetypes, file_years) {
  
  connect_ojo()
  d <- dbGetQuery(ojo_db, glue_sql(
    "SELECT court, casetype, file_year, COUNT(casenum) as n
                          FROM ojo_definfo
                          WHERE court IN ({courts*})
                          AND casetype IN ({casetypes*})
                          AND file_year IN ({file_years*})
    GROUP BY court, casetype, file_year", 
    .con = ojo_db)) 
  disconnect_ojo()
  
  for (c in courts) {
    for (t in casetypes) {
      for (y in file_years) {
        
        rows_available <- nrow(d[d$court == c & d$casetype == t & d$file_year == y, ])
        
        if (rows_available > 0) {
          print(
            paste("Data already compiled for", c, t, y
            )
          )
        } else {
          print(
            paste(
              "Compiling defendant information for", c, t, y
            )
          )
          
          if (c %in% oscn_counties) {
            oscn_compile_definfo(c, t, y)
          } else if (c %in% odcr_counties) {
            odcr_compile_definfo(c, t, y)
          }
          
        }
      }
    }
  }
  
  connect_ojo()
  df <- dbGetQuery(ojo_db, glue_sql(
    "SELECT court, casenum, casetype, file_year, def_id, defname, sex_imputed, def_address, def_zip, def_mob
                          FROM ojo_definfo
                          WHERE court IN ({courts*})
                          AND casetype IN ({casetypes*})
                          AND file_year IN ({file_years*})", 
    .con = ojo_db)) 
  disconnect_ojo()
  
  return(df)
}
