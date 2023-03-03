#' @title OJO Auth
#' 
#' @description Configure credentials for the Open Justice Oklahoma database
#'
#' @details
#' Assists the user in populating a .Renviron file with the necessary environment variables to connect to the Open Justice Oklahoma database.
#'
#' @param host The host name of the database server
#' @param port The port number of the database server
#' @param username The username to use to connect to the database
#' @param password The password to use to connect to the database
#' @param .admin A logical value indicating whether to connect to the database as an administrator
#' @param .overwrite A logical value indicating whether to overwrite the existing .Renviron file
#' @param .install A logical value indicating whether to install the database connection or use it only for the current session
#'
#' @export
#' @returns Nothing
#'
#' @examples
#' \dontrun{
#' ojo_auth()
#' }
#' @section Side Effects:
#' The first time this function is run, it will prompt the user for a username, password, and host name.
#' It will then store these credentials in the user's .Renviron file.
#' If the .Renviron file already exists, it will be backed up and the new credentials will be appended to the end of the file.
#' If the .Renviron file does not exist, it will be created and the credentials will be stored there.
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
          dplyr::filter(!stringr::str_detect(V1, "(OJO_HOST)|(OJO_PORT)|(OJO_DRIVER)|(OJO_SSL)"))
        if (.admin == T) {
          newenv <- newenv |>
            dplyr::filter(!stringr::str_detect(V1, "(OJO_ADMIN_USER)|(OJO_ADMIN_PASS)")) |>
            as.data.frame()
        } else {
          newenv <- newenv |>
            dplyr::filter(!stringr::str_detect(V1, "(OJO_DEFAULT_USER)|(OJO_DEFAULT_PASS)")) |>
            as.data.frame()
        }
        utils::write.table(newenv, renv,
          quote = FALSE, sep = "\n",
          col.names = FALSE, row.names = FALSE
        )
      } else {
        tv <- readLines(renv)
        if (.admin) {
          if (any(grepl("OJO_ADMIN_USER", tv))) {
            stop("An OJO_ADMIN_USER already exists. You can overwrite it with the argument `.overwrite = TRUE`", call. = F)
          }
        } else {
          if (any(grepl("OJO_DEFAULT_USER", tv))) {
            stop("An OJO_DEFAULT_USER already exists. You can overwrite it with the argument `.overwrite = TRUE`", call. = FALSE)
          }
        }
      }
    }
    hostconcat <- paste0("OJO_HOST='", host, "'")
    portconcat <- paste0("OJO_PORT='", port, "'")
    if (.admin) {
      userconcat <- paste0("OJO_ADMIN_USER='", username, "'")
      passconcat <- paste0("OJO_ADMIN_PASS='", password, "'")
    } else {
      userconcat <- paste0("OJO_DEFAULT_USER='", username, "'")
      passconcat <- paste0("OJO_DEFAULT_PASS='", password, "'")
    }
    driverconcat <- paste0("OJO_DRIVER='PostgreSQL Driver'")
    sslmodeconcat <- paste0("OJO_SSL_MODE='verify-ca'")
    rootcertconcat <- paste0("OJO_SSL_ROOT_CERT='", rootcert, "'")
    clientcertconcat <- paste0("OJO_SSL_CERT='", clientcert, "'")
    clientkeyconcat <- paste0("OJO_SSL_KEY='", clientkey, "'")
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
    Sys.setenv(OJO_HOST = host)
    Sys.setenv(OJO_PORT = port)
    if (.admin == T) {
      Sys.setenv(OJO_ADMIN_USER = username)
      Sys.setenv(OJO_ADMIN_PASS = password)
    } else {
      Sys.setenv(OJO_DEFAULT_USER = username)
      Sys.setenv(OJO_DEFAULT_PASS = password)
    }
    Sys.setenv(OJO_DRIVER = "PostgreSQL Driver")
    Sys.setenv(OJO_SSL_MODE = "verify-ca")
    Sys.setenv(OJO_SSL_ROOT_CERT = rootcert)
    Sys.setenv(OJO_SSL_CERT = clientcert)
    Sys.setenv(OJO_SSL_KEY = clientkey)
  }
  invisible()
}