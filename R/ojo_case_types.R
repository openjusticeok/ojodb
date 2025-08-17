#' List the case types present in the OJO database
#'
#' Returns a tibble containing all the case types present in the OJO database
#'
#' @export ojo_case_types
#' @return Tibble of case types
#' @examples
#'
#' \dontrun{
#' ojo_case_types()
#'}
#'
ojo_case_types <- function() {
  ojo_tbl("case") |>
    dplyr::count(case_type, sort = T) |>
    dplyr::collect()
}
