#' Add party details to a parties query
#'
#' Query the Open Justice Oklahoma database for party details
#'
#' @param data A lazy tibble containing the results of a parties query
#' @param vars A character vector of variables to return
#' @param con The OJO database connection to use
#' @param ... Placeholder for additional arguments
#'
#' @export ojo_add_party_details
#' @return data, a lazy tibble containing the resulting party details
#' @examples
#' \dontrun{
#' ojo_add_party_details()
#' }
#'
ojo_add_party_details <- function(data, vars = NULL, con = NULL, ...) {
  if (!inherits(data, "tbl_lazy")) {
    stop("Don't use `collect()` before this function")
  }

  columns <- colnames(data)

  if (!"party" %in% columns) {
    if ("parties" %in% columns) {
      stop(
        "Data contains a `parties` column but no `party` column. ",
        "You must first unnest the `parties` column into a column named `party`."
      )
    }
    stop("Data must contain a column named `party`.")
  }

  data <- data |>
    dplyr::left_join(
      ojo_tbl("person_record", con = con),
      by = c("party" = "id")
    )

  return(data)
}
