doc_current_pop <- function() {

  connect_ojo()
  d <- ojo_tbl("doc_profile") %>%
    filter(status == "Active") %>%
    collect() %>%
    mutate(inside = str_detect(facility, "CORR|PENIT|RECEPT") & !str_detect(facility, "ESCAPE"))
  disconnect_ojo()

  return(d)
}
