#' Query the OJO database for all active DOC inmates and supervisees
#'
#' Queries the doc_profile table in the OJO database and returns a dataframe with all information available for people listed as active. Also defines a logical variable "inside" that indicates whether the person is incarcerated (TRUE) or under supervision (FALSE).
#'
#' @return A dataframe
#' @examples
#' \dontrun{
#' d <- doc_current_pop()
#'}

doc_current_pop <- function() {

  connect_ojo()
  d <- ojo_tbl("doc_profile") %>%
    filter(status == "Active") %>%
    collect() %>%
    mutate(inside = str_detect(facility, "CORR|PENIT|RECEPT|REFORM") & !str_detect(facility, "ESCAPE"))
  disconnect_ojo()

  return(d)
}
