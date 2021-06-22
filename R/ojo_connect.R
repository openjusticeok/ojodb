#' Connect to the Open Justice Oklahoma database
#'
#' Opens a connection to the Open Justice Oklahoma database using credentials stored in the .Renviron file. If no credentials exist, prompts for user, password, and host name and provides instructions to store them for future sessions.
#'
#' @aliases connect_ojo ojo_connect
#' @aliases ojo_auth
#' @export ojo_connect connect_ojo ojo_auth
#' @return ojo_db, a database connection object
#' @examples
#' \dontrun{
#' ojo_connect()
#' ojo_auth()
#' }
#' @section Aliases:
#'  For comfort, `ojo_connect` and `connect_ojo` can be used interchangeably.
#' @seealso ojo_auth()

ojo_auth <- function(host, port, username, password, .admin = F, .overwrite = T, .install = T) {
  if (.install) {
    home <- Sys.getenv("HOME")
    renv <- file.path(home, ".Renviron")
    if(file.exists(renv)) {
      # Backup original .Renviron before doing anything else here.
      file.copy(renv, file.path(home, ".Renviron_backup"))
    }
    if(!file.exists(renv)){
      file.create(renv)
    } else{
      if(isTRUE(.overwrite)){
        message("Your original .Renviron will be backed up and stored in your R HOME directory if needed.")
        oldenv <- read.table(renv, stringsAsFactors = FALSE)
        newenv <- oldenv |>
          as_tibble() |>
          filter(!str_detect(V1, "(NEW_OJO_HOST)|(NEW_OJO_PORT)"))
        if(.admin == T) {
          newenv <- newenv |>
            filter(!str_detect(V1, "(NEW_OJO_ADMIN_USER)|(NEW_OJO_ADMIN_PASS)")) |>
            as.data.frame()
        } else {
          newenv <- newenv |>
            filter(!str_detect(V1, "(NEW_OJO_DEFAULT_USER)|(NEW_OJO_DEFAULT_PASS)")) |>
            as.data.frame()
        }
        write.table(newenv, renv, quote = FALSE, sep = "\n",
                    col.names = FALSE, row.names = FALSE)
      } else{
        tv <- readLines(renv)
        if(.admin == T) {
          if(any(grepl("NEW_OJO_ADMIN_USER", tv))) {
            stop("An OJO_ADMIN_USER already exists. You can overwrite it with the argument `.overwrite = TRUE`", call. = F)
          }
        } else {
          if(any(grepl("NEW_OJO_DEFAULT_USER",tv))) {
            stop("An OJO_DEFAULT_USER already exists. You can overwrite it with the argument `.overwrite = TRUE`", call.=FALSE)
          }
        }
      }
    }
    hostconcat <- paste0("NEW_OJO_HOST='", host, "'")
    portconcat <- paste0("NEW_OJO_PORT='", port, "'")
    if(.admin == T) {
      userconcat <- paste0("NEW_OJO_ADMIN_USER='", username, "'")
      passconcat <- paste0("NEW_OJO_ADMIN_PASS='", password, "'")
    } else {
      userconcat <- paste0("NEW_OJO_DEFAULT_USER='", username, "'")
      passconcat <- paste0("NEW_OJO_DEFAULT_PASS='", password, "'")
    }
    write(hostconcat, renv, sep = "\n", append = TRUE)
    write(portconcat, renv, sep = "\n", append = TRUE)
    write(userconcat, renv, sep = "\n", append = TRUE)
    write(passconcat, renv, sep = "\n", append = TRUE)
    message('Your configuration has been stored in your .Renviron. \nTo use now, restart R or run `readRenviron("~/.Renviron")`')
    invisible()
  } else {
    message("To install your configuration for use in future sessions, run this function with `.install = TRUE`.")
    Sys.setenv(NEW_OJO_HOST = host)
    Sys.setenv(NEW_OJO_PORT = port)
    if(.admin == T) {
      Sys.setenv(NEW_OJO_ADMIN_USER = username)
      Sys.setenv(NEW_OJO_ADMIN_PASS = password)
    } else {
      Sys.setenv(NEW_OJO_DEFAULT_USER = username)
      Sys.setenv(NEW_OJO_DEFAULT_PASS = password)
    }
  }
  invisible()
}

ojo_connect <- function(.admin = F) {
  if(.admin == T) {
    if(Sys.getenv("NEW_OJO_ADMIN") == "") {
      message("No admin configuration for the OJO database was found. Please create one now using `ojo_auth(.admin = T)`.")
    } else {
      ojodb <- pool::dbPool(
        drv = RPostgres::Postgres(),
        host = Sys.getenv("NEW_OJO_HOST"),
        dbname = "ojo",
        port = Sys.getenv("NEW_OJO_PORT"),
        user = Sys.getenv("NEW_OJO_ADMIN_USER"),
        password = Sys.getenv("NEW_OJO_ADMIN_PASS")
      )
      return(ojodb)
    }
  } else {
    if(Sys.getenv("NEW_OJO_HOST") == "") {
      message("No configuration for the OJO database was found. Please create one now using `ojo_auth()`.")
      invisible()
    } else {
      ojodb <- pool::dbPool(
        drv = RPostgres::Postgres(),
        host = Sys.getenv("NEW_OJO_HOST"),
        dbname = "ojo",
        port = Sys.getenv("NEW_OJO_PORT"),
        user = Sys.getenv("NEW_OJO_DEFAULT_USER"),
        password = Sys.getenv("NEW_OJO_DEFAULT_PASS")
      )
      return(ojodb)
    }
  }
}

connect_ojo <- ojo_connect

