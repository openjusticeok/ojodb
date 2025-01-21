#' @title Create configuration for OJO database connection
#'
#' @description Configure credentials for the Open Justice Oklahoma database
#'
#' @details
#' Assists the user in populating a .Renviron file with the necessary environment variables to connect to the Open Justice Oklahoma database.
#'
#' @param ...
#'   \describe{
#'     \item{\code{host}}{The host name of the database server.}
#'     \item{\code{port}}{The port number of the database server.}
#'     \item{\code{username}}{The username to use to connect to the database.}
#'     \item{\code{password}}{The password to use to connect to the database.}
#'     \item{\code{sslmode}}{Optional. The SSL mode to use for connections. Defaults to `verify-ca`.}
#'     \item{\code{sslrootcert}}{Optional. The file path to the root SSL certificate. Defaults to `~/.postgresql/ojodb/server-ca.pem`.}
#'     \item{\code{sslcert}}{Optional. The file path to the client SSL certificate. Defaults to `~/.postgresql/ojodb/client-cert.pem`.}
#'     \item{\code{sslkey}}{Optional. The file path to the client SSL key. Defaults to `~/.postgresql/ojodb/client-key.pem`.}
#'   }
#' @param .admin A logical value indicating whether to connect to the database as an administrator
#' @param .overwrite A logical value indicating whether to overwrite the existing .Renviron file
#' @param .install A logical value indicating whether to install the database connection or use it only for the current session
#'
#' @export
#' @returns Nothing
#'
#' @examples
#' \dontrun{
#' # Using default SSL certificate paths
#' ojo_auth(
#'   host = "db.example.com",
#'   port = 5432,
#'   username = "user",
#'   password = "pass"
#' )
#'
#' # Using custom SSL certificate paths
#' ojo_auth(
#'   host = "db.example.com",
#'   port = 5432,
#'   username = "user",
#'   password = "pass",
#'   sslrootcert = "/custom/path/server-ca.pem",
#'   sslcert = "/custom/path/client-cert.pem",
#'   sslkey = "/custom/path/client-key.pem"
#' )
#' }
#'
#' @section Side Effects:
#' The first time this function is run, it will prompt the user for a username, password, and host name.
#' It will then store these credentials in the user's .Renviron file.
#' If the .Renviron file already exists, it will be backed up and the new credentials will be appended to the end of the file.
#' If the .Renviron file does not exist, it will be created and the credentials will be stored there.
#'
ojo_auth <- function(..., .admin = F, .overwrite = T, .install = T) {
  args <- list(...)

  # Required primary arguments
  required_args <- c("host", "port", "username", "password")

  missing_args <- setdiff(required_args, names(args))

  if (length(missing_args) > 0) {
    rlang::abort(sprintf("Missing required argument(s): %s", paste(missing_args, collapse = ", ")))
  }

  # Assign primary arguments
  host <- args$host
  port <- args$port
  username <- args$username
  password <- args$password

  # Optional SSL certificate arguments with defaults
  sslrootcert <- if (!is.null(args$rootcert)) args$rootcert else "~/.postgresql/ojodb/server-ca.pem"
  sslcert <- if (!is.null(args$clientcert)) args$clientcert else "~/.postgresql/ojodb/client-cert.pem"
  sslkey <- if (!is.null(args$clientkey)) args$clientkey else "~/.postgresql/ojodb/client-key.pem"

  # Resolve absolute paths
  sslrootcert_abs <- fs::path_abs(sslrootcert)
  sslcert_abs <- fs::path_abs(sslcert)
  sslkey_abs <- fs::path_abs(sslkey)

  # Define .Renviron path
  home <- fs::path_home()
  renv <- fs::path(home, ".Renviron")

  # Check if SSL certs are in correct location; if not, throw error
  if (!fs::file_exists(sslrootcert_abs) |
     !fs::file_exists(sslcert_abs) |
     !fs::file_exists(sslkey_abs)) {
    rlang::abort(
      paste0(
        "SSL certs could not be found at the specified location (",
        sslrootcert_abs,
        ").\nPlease check that you have all three (server-ca.pem, client-cert.pem, and client-key.pem) in the specified location."
      )
    )
  }

  if (.install) {
    # Backup existing .Renviron with timestamp
    if (fs::file_exists(renv)) {
      timestamp <- format(Sys.time(), "%Y%m%d%H%M%S")
      backup_renv <- fs::path(home, sprintf(".Renviron_backup_%s", timestamp))
      fs::file_copy(renv, backup_renv, overwrite = TRUE)
      cli::cli_alert_info("Existing `.Renviron` backed up to `%s`.", backup_renv)
    }

    # Read existing .Renviron content if exists
    if (fs::file_exists(renv)) {
      existing_env <- readLines(renv, warn = FALSE)
    } else {
      existing_env <- character()
      fs::file_create(renv)
    }

    # Define regex patterns for OJO variables
    ojo_patterns <- c(
      "^OJO_HOST=",
      "^OJO_PORT=",
      "^OJO_ADMIN_USER=",
      "^OJO_ADMIN_PASS=",
      "^OJO_DEFAULT_USER=",
      "^OJO_DEFAULT_PASS=",
      "^OJO_SSL_MODE=",
      "^OJO_SSL_ROOT_CERT=",
      "^OJO_SSL_CERT=",
      "^OJO_SSL_KEY="
    )

    # Remove existing OJO variables
    filtered_env <- existing_env[!grepl(paste(ojo_patterns, collapse = "|"), existing_env)]

    # Prepare new environment variables
    new_env_vars <- c(
      sprintf("OJO_HOST='%s'", host),
      sprintf("OJO_PORT='%s'", port),
      if (.admin) {
        c(
          sprintf("OJO_ADMIN_USER='%s'", username),
          sprintf("OJO_ADMIN_PASS='%s'", password)
        )
      } else {
        c(
          sprintf("OJO_DEFAULT_USER='%s'", username),
          sprintf("OJO_DEFAULT_PASS='%s'", password)
        )
      },
      "OJO_SSL_MODE='verify-ca'",
      sprintf("OJO_SSL_ROOT_CERT='%s'", rootcert_abs),
      sprintf("OJO_SSL_CERT='%s'", clientcert_abs),
      sprintf("OJO_SSL_KEY='%s'", clientkey_abs)
    )

    # Combine filtered existing env with new env vars
    updated_env <- c(filtered_env, new_env_vars)

    # Write updated environment variables back to .Renviron
    writeLines(updated_env, renv, useBytes = TRUE)

    cli::cli_alert_success(
      "OJO configuration has been successfully stored in `.Renviron`.
      To apply changes, restart R or run `readRenviron('~/.Renviron')`."
    )

    # ...if not, create a fresh one.
    if (!fs::file_exists(renv)) {
      fs::file_create(renv)
    # Filling out the .Renviron file
    } else {

      # If we want to overwrite the old config:
      if (isTRUE(.overwrite)) {
        cli::cli_alert_info("Your original .Renviron will be backed up and stored in your R HOME directory if needed.")

        # Saving the original .Renviron file
        oldenv <- utils::read.table(renv, stringsAsFactors = FALSE)
        # Creating the new .Renviron file (not filled out yet, all OJO variables removed)
        newenv <- oldenv |>
          dplyr::as_tibble() |>
          dplyr::filter(!stringi::stri_detect_regex(.data$V1, "(OJO_HOST)|(OJO_PORT)|(OJO_DRIVER)|(OJO_SSL)"))
        if (.admin == T) {
          newenv <- newenv |>
            dplyr::filter(!stringi::stri_detect_regex(.data$V1, "(OJO_ADMIN_USER)|(OJO_ADMIN_PASS)")) |>
            as.data.frame()
        } else {
          newenv <- newenv |>
            dplyr::filter(!stringi::stri_detect_regex(.data$V1, "(OJO_DEFAULT_USER)|(OJO_DEFAULT_PASS)")) |>
            as.data.frame()
        }

        # Save new .Renviron file with OJO variables removed
        utils::write.table(newenv, renv,
          quote = FALSE, sep = "\n",
          col.names = FALSE, row.names = FALSE
        )

      # If a config already exists, and we don't want to overwrite it:
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

    # Fill out .Renviron with new arguments
    hostconcat <- paste0("OJO_HOST='", host, "'")
    portconcat <- paste0("OJO_PORT='", port, "'")
    if (.admin) {
      userconcat <- paste0("OJO_ADMIN_USER='", username, "'")
      passconcat <- paste0("OJO_ADMIN_PASS='", password, "'")
    } else {
      userconcat <- paste0("OJO_DEFAULT_USER='", username, "'")
      passconcat <- paste0("OJO_DEFAULT_PASS='", password, "'")
    }
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
    cli::cli_alert_success('Your configuration has been stored in your .Renviron.
                           To use now, restart R or run `readRenviron("~/.Renviron")`')
    invisible()

  # If .install = FALSE...
  } else {
    cli::cli_alert_info("To install your configuration for use in future sessions, run this function with `.install = TRUE`.")

    # ...set up local environment, but don't save to .Renviron
    Sys.setenv(OJO_HOST = host)
    Sys.setenv(OJO_PORT = port)
    if (.admin == T) {
      Sys.setenv(OJO_ADMIN_USER = username)
      Sys.setenv(OJO_ADMIN_PASS = password)
    } else {
      Sys.setenv(OJO_DEFAULT_USER = username)
      Sys.setenv(OJO_DEFAULT_PASS = password)
    }
    Sys.setenv(OJO_SSL_MODE = "verify-ca")
    Sys.setenv(OJO_SSL_ROOT_CERT = rootcert)
    Sys.setenv(OJO_SSL_CERT = clientcert)
    Sys.setenv(OJO_SSL_KEY = clientkey)
  }
  invisible()
}
