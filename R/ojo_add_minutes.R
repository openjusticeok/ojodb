#' Query minutes for a given case
#'
#' Query the Open Justice Oklahoma database for the minutes of a case
#'
#' @param data A lazy tibble containing the cases to query
#' @param con The OJO database connection to use
#' @param ... Placeholder for additional arguments
#'
#' @export ojo_add_minutes
#' @returns A lazy tibble containing the resulting cases with minutes
#'
#' @examples
#' \dontrun{
#' ojo_add_minutes()
#' }
#'
ojo_add_minutes <- function(data, con = NULL, ...) {
  minutes <- ojo_tbl("minute", con = con)

  data <- data |>
    dplyr::left_join(
      minutes,
      by = c("id" = "case_id"),
      suffix = c("", ".minute")
    )

  return(data)
}
