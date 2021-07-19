#' List the case types present in the OJO database
#'
#' Returns a tibble containing all the case types present in the OJO database
#'
#' @return Tibble of case types
#' @examples
#'
#' ojo_case_types()
#'

ojo_case_types <- function() {
  ojo_table("case") |>
    count(case_type, sort = T) |>
    collect()
}
