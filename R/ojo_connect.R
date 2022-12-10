#' Connect to the Open Justice Oklahoma database
#'
#' Opens a connection to the Open Justice Oklahoma database using credentials stored in the .Renviron file.
#' If no credentials exist, prompts for user, password, and host name and provides instructions to store them for future sessions.
#'
#' @aliases connect_ojo ojo_connect
#' @aliases ojo_auth
#' 
#' @param host The host name of the database server
#' @param port The port number of the database server
#' @param username The username to use to connect to the database
#' @param password The password to use to connect to the database
#' @param .admin A logical value indicating whether to connect to the database as an administrator
#' @param .overwrite A logical value indicating whether to overwrite the existing .Renviron file
#' @param .install A logical value indicating whether to install the database connection or use it only for the current session  
#'
#' @export ojo_connect connect_ojo ojo_auth
#' @return ojo_db, a database connection object
#'
#' @examples
#' \dontrun{
#' ojo_connect()
#' ojo_auth()
#' }
#' @section Side Effects:
#' The first time this function is run, it will prompt the user for a username, password, and host name.
#' It will then store these credentials in the user's .Renviron file.
#' If the .Renviron file already exists, it will be backed up and the new credentials will be appended to the end of the file.
#' If the .Renviron file does not exist, it will be created and the credentials will be stored there.
#' 
#' @section Aliases:
#'  For comfort, `ojo_connect` and `connect_ojo` can be used interchangeably.
#' @seealso ojo_auth()
#' 
ojo_auth <- function(host, port, username, password, .admin = F, .overwrite = T, .install = T) {
  home <- Sys.getenv("HOME")
  renv <- file.path(home, ".Renviron")
  rootcert <- file.path(home, ".postgresql/ojodb/server-ca.pem")
  clientcert <- file.path(home, ".postgresql/ojodb/client-cert.pem")
  clientkey <- file.path(home, ".postgresql/ojodb/client-key.pk8")
  if (.install) {
    if (file.exists(renv)) {
      # Backup original .Renviron before doing anything else here.
      file.copy(renv, file.path(home, ".Renviron_backup"))
    }
    if (!file.exists(renv)) {
      file.create(renv)
    } else {
      if (isTRUE(.overwrite)) {
        message("Your original .Renviron will be backed up and stored in your R HOME directory if needed.")
        oldenv <- utils::read.table(renv, stringsAsFactors = FALSE)
        newenv <- oldenv |>
          dplyr::as_tibble() |>
          dplyr::filter(!stringr::str_detect(V1, "(NEW_OJO_HOST)|(NEW_OJO_PORT)|(NEW_OJO_DRIVER)|(NEW_OJO_SSL)"))
        if (.admin == T) {
          newenv <- newenv |>
            dplyr::filter(!stringr::str_detect(V1, "(NEW_OJO_ADMIN_USER)|(NEW_OJO_ADMIN_PASS)")) |>
            as.data.frame()
        } else {
          newenv <- newenv |>
            dplyr::filter(!stringr::str_detect(V1, "(NEW_OJO_DEFAULT_USER)|(NEW_OJO_DEFAULT_PASS)")) |>
            as.data.frame()
        }
        utils::write.table(newenv, renv,
          quote = FALSE, sep = "\n",
          col.names = FALSE, row.names = FALSE
        )
      } else {
        tv <- readLines(renv)
        if (.admin == T) {
          if (any(grepl("NEW_OJO_ADMIN_USER", tv))) {
            stop("An OJO_ADMIN_USER already exists. You can overwrite it with the argument `.overwrite = TRUE`", call. = F)
          }
        } else {
          if (any(grepl("NEW_OJO_DEFAULT_USER", tv))) {
            stop("An OJO_DEFAULT_USER already exists. You can overwrite it with the argument `.overwrite = TRUE`", call. = FALSE)
          }
        }
      }
    }
    hostconcat <- paste0("NEW_OJO_HOST='", host, "'")
    portconcat <- paste0("NEW_OJO_PORT='", port, "'")
    if (.admin == T) {
      userconcat <- paste0("NEW_OJO_ADMIN_USER='", username, "'")
      passconcat <- paste0("NEW_OJO_ADMIN_PASS='", password, "'")
    } else {
      userconcat <- paste0("NEW_OJO_DEFAULT_USER='", username, "'")
      passconcat <- paste0("NEW_OJO_DEFAULT_PASS='", password, "'")
    }
    driverconcat <- paste0("NEW_OJO_DRIVER='PostgreSQL Driver'")
    sslmodeconcat <- paste0("NEW_OJO_SSL_MODE='verify-ca'")
    rootcertconcat <- paste0("NEW_OJO_SSL_ROOT_CERT='", rootcert, "'")
    clientcertconcat <- paste0("NEW_OJO_SSL_CERT='", clientcert, "'")
    clientkeyconcat <- paste0("NEW_OJO_SSL_KEY='", clientkey, "'")
    write(hostconcat, renv, sep = "\n", append = TRUE)
    write(portconcat, renv, sep = "\n", append = TRUE)
    write(userconcat, renv, sep = "\n", append = TRUE)
    write(passconcat, renv, sep = "\n", append = TRUE)
    write(sslmodeconcat, renv, sep = "\n", append = TRUE)
    write(rootcertconcat, renv, sep = "\n", append = TRUE)
    write(clientcertconcat, renv, sep = "\n", append = TRUE)
    write(clientkeyconcat, renv, sep = "\n", append = TRUE)
    message('Your configuration has been stored in your .Renviron. \nTo use now, restart R or run `readRenviron("~/.Renviron")`')
    invisible()
  } else {
    message("To install your configuration for use in future sessions, run this function with `.install = TRUE`.")
    Sys.setenv(NEW_OJO_HOST = host)
    Sys.setenv(NEW_OJO_PORT = port)
    if (.admin == T) {
      Sys.setenv(NEW_OJO_ADMIN_USER = username)
      Sys.setenv(NEW_OJO_ADMIN_PASS = password)
    } else {
      Sys.setenv(NEW_OJO_DEFAULT_USER = username)
      Sys.setenv(NEW_OJO_DEFAULT_PASS = password)
    }
    Sys.setenv(NEW_OJO_DRIVER = "PostgreSQL Driver")
    Sys.setenv(NEW_OJO_SSL_MODE = "verify-ca")
    Sys.setenv(NEW_OJO_SSL_ROOT_CERT = rootcert)
    Sys.setenv(NEW_OJO_SSL_CERT = clientcert)
    Sys.setenv(NEW_OJO_SSL_KEY = clientkey)
  }
  invisible()
}

ojo_connect <- function(.admin = F) {
  if (.admin == T) {
    if (Sys.getenv("NEW_OJO_ADMIN") == "") {
      message("No admin configuration for the OJO database was found. Please create one now using `ojo_auth(.admin = T)`.")
    } else {
      ojodb <- pool::dbPool(
        drv = odbc::odbc(),
        Driver = Sys.getenv("NEW_OJO_DRIVER"),
        Server = Sys.getenv("NEW_OJO_HOST"),
        Database = "ojodb",
        Port = Sys.getenv("NEW_OJO_PORT"),
        Username = Sys.getenv("NEW_OJO_ADMIN_USER"),
        Password = Sys.getenv("NEW_OJO_ADMIN_PASS"),
        SSLmode = Sys.getenv("NEW_OJO_SSL_MODE"),
        Pqopt = stringr::str_glue('{sslrootcert={{Sys.getenv("NEW_OJO_SSL_ROOT_CERT")}} sslcert={{Sys.getenv("NEW_OJO_SSL_CERT")}} sslkey={{Sys.getenv("NEW_OJO_SSL_KEY")}}}', .open = "{{", .close = "}}")
      )
      assign("ojodb", ojodb, envir = .GlobalEnv)
      invisible()
    }
  } else {
    if (Sys.getenv("NEW_OJO_HOST") == "") {
      message("No configuration for the OJO database was found. Please create one now using `ojo_auth()`.")
      invisible()
    } else {
      ojodb <- pool::dbPool(
        drv = odbc::odbc(),
        Driver = Sys.getenv("NEW_OJO_DRIVER"),
        Server = Sys.getenv("NEW_OJO_HOST"),
        Database = "ojodb",
        Port = Sys.getenv("NEW_OJO_PORT"),
        Username = Sys.getenv("NEW_OJO_DEFAULT_USER"),
        Password = Sys.getenv("NEW_OJO_DEFAULT_PASS"),
        SSLmode = Sys.getenv("NEW_OJO_SSL_MODE"),
        Pqopt = stringr::str_glue('{sslrootcert={{Sys.getenv("NEW_OJO_SSL_ROOT_CERT")}} sslcert={{Sys.getenv("NEW_OJO_SSL_CERT")}} sslkey={{Sys.getenv("NEW_OJO_SSL_KEY")}}}', .open = "{{", .close = "}}")
      )
      assign("ojodb", ojodb, envir = .GlobalEnv)
      invisible()
    }
  }
}

connect_ojo <- ojo_connect
