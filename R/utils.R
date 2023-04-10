#' @title OJO Environment
#'
#' @description Get the environment of the OJO package, which holds the database pool object.
#'
#' @param ... Placeholder for future arguments
#' 
#' @export
#'
ojo_env <- function(...) {
  .ojo_env
}

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

#' @title Skip If No Database Connection
#' 
#' @description
#' Skips a test if the `ojodb` package is not installed or if the user is not connected to the OJO database.
#'
skip_if_no_db <- function() {
  testthat::skip("No database connection available")
}