#' @title OJO Version
#'
#' @description
#' Returns the version of the `ojodb` package.
#'
#' @param ... Placeholder for future arguments
#'
#' @export
#' @returns A character vector containing the version number of the `ojodb` package.
#'
ojo_version <- function(...) {
  utils::packageVersion("ojodb")
}
