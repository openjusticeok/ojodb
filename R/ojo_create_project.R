#' @title Create Project
#' @description Create a new project
#' 
#' @param name Name of the project
#' 
#' @export
#' @returns
#' 
ojo_create_project <- function(name, ..., .private = TRUE) {
  # fs::dir_create("./test/")
  # fs::file_create("./test/test.txt")

  
  project_dir <- fs::dir_create(name)

  usethis::create_project(project_dir, rstudio = TRUE, open = FALSE)

  setwd(project_dir)

  usethis::use_readme_rmd()
  usethis::use_gpl3_license()
  renv::init(bare = TRUE)

  usethis::use_git(message = "Initial commit")
  usethis::use_github(
    organisation = "openjusticeok",
    private = .private
  )
}