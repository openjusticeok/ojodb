#' Connect to the Open Justice Oklahoma database
#'
#' Opens a connection to the Open Justice Oklahoma database using credentials stored in the .Renviron file. If no credentials exist, prompts for user, password, and host name and provides instructions to store them for future sessions.
#'
#' @aliases connect_ojo ojo_connect
#' @aliases ojo_auth
#' @export connect_ojo ojo_connect ojo_auth
#' @return ojo_db, a database connection object
#' @examples
#' \dontrun{
#' connect_ojo()
#' ojo_auth()
#' }
#' @section Aliases:
#'  For comfort, `connect_ojo` and `ojo_connect` can be used interchangeably.
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
        oldenv=read.table(renv, stringsAsFactors = FALSE)
        newenv <- oldenv[-grep("OJO_HOST", oldenv),]
        newenv <- newenv[-grep("OJO_PORT", newenv),]
        if(.admin == T) {
          newenv <- newenv[-grep("OJO_ADMIN_USER", newenv),]
          newenv <- newenv[-grep("OJO_ADMIN_PASS", newenv),]
        } else {
          newenv <- newenv[-grep("OJO_DEFAULT_USER", newenv),]
          newenv <- newenv[-grep("OJO_DEFAULT_PASS", newenv),]
        }
        write.table(newenv, renv, quote = FALSE, sep = "\n",
                    col.names = FALSE, row.names = FALSE)
      } else{
        tv <- readLines(renv)
        if(.admin == T) {
          if(any(grepl("OJO_ADMIN_USER", tv))) {
            stop("An OJO_ADMIN_USER already exists. You can overwrite it with the argument `.overwrite = TRUE`", call. = F)
          }
        } else {
          if(any(grepl("OJO_DEFAULT_USER",tv))) {
            stop("An OJO_DEFAULT_USER already exists. You can overwrite it with the argument `.overwrite = TRUE`", call.=FALSE)
          }
        }
      }
    }
    hostconcat <- paste0("OJO_HOST='", host, "'")
    portconcat <- paste0("OJO_PORT='", port, "'")
    if(.admin == T) {
      userconcat <- paste0("OJO_ADMIN_USER='", username, "'")
      passconcat <- paste0("OJO_ADMIN_PASS='", password, "'")
    } else {
      userconcat <- paste0("OJO_DEFAULT_USER='", username, "'")
      passconcat <- paste0("OJO_DEFAULT_PASS='", password, "'")
    }
    write(hostconcat, renv, sep = "\n", append = TRUE)
    write(portconcat, renv, sep = "\n", append = TRUE)
    write(userconcat, renv, sep = "\n", append = TRUE)
    write(passconcat, renv, sep = "\n", append = TRUE)
    message('Your configuration has been stored in your .Renviron. \nTo use now, restart R or run `readRenviron("~/.Renviron")`')
    return()
  } else {
    message("To install your configuration for use in future sessions, run this function with `.install = TRUE`.")
    Sys.setenv(OJO_HOST = host)
    Sys.setenv(OJO_PORT = port)
    if(.admin == T) {
      Sys.setenv(OJO_ADMIN_USER = username)
      Sys.setenv(OJO_ADMIN_PASS = password)
    } else {
      Sys.setenv(OJO_DEFAULT_USER = username)
      Sys.setenv(OJO_DEFAULT_PASS = password)
    }
  }
}

connect_ojo <- function(.admin = F) {
  if(.admin == T) {
    if(Sys.getenv("OJO_ADMIN") == "") {
      message("No admin configuration for the OJO database was found. Please create one now using `ojo_auth(.admin = T)`.")
    } else {
      ojodb <- dbPool(
        drv = RPostgres(),
        host = Sys.getenv("OJO_HOST"),
        dbname = "ojo",
        port = Sys.getenv("OJO_PORT"),
        user = Sys.getenv("OJO_ADMIN_USER"),
        password = Sys.getenv("OJO_ADMIN_PASS")
      )
      return(ojodb)
    }
  } else {
    if(Sys.getenv("OJO_HOST") == "") {
      message("No configuration for the OJO database was found. Please create one now using `ojo_auth()`.")
      return()
    } else {
      ojodb <- dbPool(
        drv = RPostgres(),
        host = Sys.getenv("OJO_HOST"),
        dbname = "ojo",
        port = Sys.getenv("OJO_PORT"),
        user = Sys.getenv("OJO_DEFAULT_USER"),
        password = Sys.getenv("OJO_DEFAULT_PASS")
      )
      return(ojodb)
    }
  }
}

ojo_connect <- connect_ojo
