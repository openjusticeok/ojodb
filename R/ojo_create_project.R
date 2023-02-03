#' @title Create Project
#' @description Create a new, opinionated OJO project
#'
#' @param name Name of the project
#' @param dir Path to the project
#' @param description Description of the project
#'
#' @export
#' @returns Silently returns the path to the project, if successful
#'
ojo_create_project <- function(name, dir = ".", rstudio = TRUE, open = TRUE) {
  project_path <- file.path(dir, name)
  usethis::create_project(path = project_path, rstudio = rstudio, open = open)
}


challenge_project_name <- function(name) {

}

#' @title Suggest Project Name
#' @description Suggest a project name based on a given name
#'
#' @param name Name of the project
#'
#' @export
#' @return Silently returns the suggested name, if successful
#'
suggest_project_name <- function(name) {
  assertthat::assert_that(
    assertthat::is.string(name)
  )

  assertthat::assert_that(
    stringr::str_length(name) > 0
  )

  suggested_name <- name |>
    stringr::str_replace_all(" ", "-") |>
    stringr::str_to_lower() |>
    stringr::str_remove_all("[^a-z0-9-_\\.]")

  if (stringr::str_length(suggested_name) > 30) {
    rlang::abort(
      glue::glue("The project name `{name}` is too long. Please choose a shorter name.")
    )
  }

  if (!name == suggested_name) {
    suggestion <- glue::glue("The project name `{name}` doesn't follow naming conventions. What about `{suggested_name}`?")
    rlang::warn(suggestion)
  }

  return(suggested_name)
}
