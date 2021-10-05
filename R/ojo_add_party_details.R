#' Add party details to a parties query
#'
#' Query the Open Justice Oklahoma database for party details
#'
#' @export ojo_add_party_details
#' @return data, a lazy tibble containing the resulting party details
#' @examples
#' \dontrun{
#' ojo_add_party_details()
#' }
#'

ojo_add_party_details <- function(data, vars = NULL, ...) {
  if(!"tbl_lazy" %in% class(data)) {
    stop("Don't use `collect()` before this function")
  }

  columns <- colnames(data)

  if(!"party" %in% columns) {
    if("parties" %in% columns) {
      if(class(data$parties) == "pq__text") {
        stop("You must first unnest the `parties` column")
      } else {
        stop("Make sure you unnested the `parties` column into a column named `party`")
      }
    }
    stop("Data must contain a column named `party`")
  }
#
#   if(is.null(vars)) {
#
#   } else {
#     if(vars != "all") {
#
#     }
#   }

  data <- data |>
    left_join(ojo_tbl("person_record"), by = c("party" = "id"))

  return(data)
}

