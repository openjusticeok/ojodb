#' Connect to the Open Justice Oklahoma database
#'
#' Opens a connection to the Open Justice Oklahoma database using credentials stored in the .Renviron file. If no credentials exist, prompts for user, password, and host name and provides instructions to store them for future sessions.
#'
#' @aliases connect_ojo ojo_connect
#' @aliases disconnect_ojo ojo_disconnect
#' @export connect_ojo ojo_connect disconnect_ojo ojo_disconnect
#' @return ojo_db, a database connection object
#' @examples
#' \dontrun{
#' connect_ojo()
#' disconnect_ojo()
#' }
#' @section Aliases:
#'  For comfort, `connect_ojo` and `ojo_connect` can be used interchangeably.
#'  So too can `disconnect_ojo` and `ojo_disconnect`.
#' @seealso disconnect_ojo()

connect_ojo <- function(username = "default") {

  good <- try(DBI::dbConnect(RMySQL::MySQL(),
                             host = Sys.getenv("OJO_HOST"),
                             dbname = "ojo",
                             port = 3306,
                             user = Sys.getenv("OJO_DEFAULT_USER"),
                             password = Sys.getenv("OJO_DEFAULT_PW")),
              silent = TRUE)

  if (class(good) == "try-error") {

    message("No user credentials for the OJO database were found. Please set them now.")

    while (class(good) == "try-error") {

      username <- readline(prompt = "Username:")
      pw <- readline(prompt = "Password:")
      host <- readline(prompt = "Host address:")

      Sys.setenv(OJO_DEFAULT_USER = username)
      Sys.setenv(OJO_DEFAULT_PW = pw)
      Sys.setenv(OJO_HOST = host)

      good <- try(DBI::dbConnect(RMySQL::MySQL(),
                                 host = Sys.getenv("OJO_HOST"),
                                 dbname = "ojo",
                                 port = 3306,
                                 user = Sys.getenv("OJO_DEFAULT_USER"),
                                 password = Sys.getenv("OJO_DEFAULT_PW")),
                  silent = TRUE)

      if (class(good) == "try-error") {
        message("Couldn't establish database connection. Please try again.")
      }

    }

    message(glue::glue("Success! To avoid this message in future sessions, run 'usethis::edit_r_environ()' and paste the following in the .Renviron file that pops up:
OJO_DEFAULT_USER='{username}'
OJO_DEFAULT_PW='{pw}'
OJO_HOST='{host}'"))

  } else if (username == "default") {
    ojo_db <<- good
    return(invisible())
  }

  if (username == "default") {
    ojo_db <<- DBI::dbConnect(RMySQL::MySQL(),
                              host = Sys.getenv("OJO_HOST"),
                              dbname = "ojo",
                              port = 3306,
                              user = Sys.getenv("OJO_DEFAULT_USER"),
                              password = Sys.getenv("OJO_DEFAULT_PW"))

  } else if (username == "admin") {
    ojo_db <<- DBI::dbConnect(RMySQL::MySQL(),
                              host = Sys.getenv("OJO_HOST"),
                              dbname = "ojo",
                              port = 3306,
                              user = Sys.getenv("OJO_ADMIN_USER"),
                              password = Sys.getenv("OJO_ADMIN_PW"))
  }

  return(invisible())
}


disconnect_ojo <- function() {
  lapply(dbListConnections(dbDriver(drv = "MySQL")), dbDisconnect)
}

ojo_connect <- connect_ojo
ojo_disconnect <- disconnect_ojo
