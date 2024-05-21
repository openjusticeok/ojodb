#' @title Anonymize
#' 
#' @description Anonymize one or more columns in a local tibble
#' 
#' @param .data A local tibble
#' @param cols A list of columns to anonymize
#' @param ... Placeholder for future arguments
#' @param .keep A character string passed to the `keep` argument of `dplyr::mutate()`. One of "unused" (default), "all", "used", or "none".
#' @param .after <tidy-select> Optionally, control where new columns should appear (the default is to replace the source columns with their anonymized version). See `dplyr::mutate()` for more details.
#' 
#' @export
#' @return A local tibble with anonymized columns, with names prefixed with "xx_"
#' 
#' @importFrom dplyr everything mutate across
#' @importFrom purrr map_chr
#' @importFrom digest digest
#' 
ojo_anonymize <- function(.data, cols = dplyr::everything(), ..., .keep = "unused", .after = NULL) {
  if (is.null(.after)) {
    .after <- {{cols}}
  }
  
  df <- .data |>
      dplyr::mutate(
          dplyr::across({{cols}}, ~purrr::map_chr(., ~digest::digest(., algo = "xxhash64")), .names = "xx_{.col}"),
          .keep = .keep,
          .after = .after
      )

  return(df)
}
