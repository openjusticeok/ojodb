#' List the case types present in the OJO database
#'
#' Returns a tibble containing all the case types present in the OJO database
#'
#' @param con The OJO database connection to use.
#'
#' @export ojo_case_types
#' @return Tibble of case types
#' @examples
#'
#' \dontrun{
#' ojo_case_types()
#'}
#'
ojo_case_types <- function(con = NULL) {
  ojo_tbl("case", con = con) |>
    dplyr::count(case_type, sort = T) |>
    dplyr::collect()
}
