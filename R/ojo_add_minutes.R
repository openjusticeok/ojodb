#' Query minutes for a given case
#'
#' Query the Open Justice Oklahoma database for the minutes of a case
#'
#' @export ojo_add_minutes
#' @return data, a lazy tibble containing the resulting cases with minutes
#' @examples
#' \dontrun{
#' ojo_add_minutes()
#' }
#'

ojo_add_minutes <- function(data, ...) {
  minutes <- ojo_tbl("minute")

  data <- data |>
    left_join(minutes,
              by = c("id" = "case_id"),
              suffix = c("", ".minute"))

  return(data)
}


