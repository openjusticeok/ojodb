# This function should take in an arbitrary amount of expressions that wil be applied to the query as dplyr filter statements
ojo_cases <- function(...) {
  data <- ojo_tbl("case") |>
    dplyr::filter(...)

  return(data)
}
