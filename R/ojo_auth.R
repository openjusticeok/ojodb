#' @title Read configuration from multiple sources
#'
#' @description Reads OJO database configuration from multiple sources with precedence
#'
#' @param args Named list of function arguments
#' @param config_file Path to YAML configuration file
#' @param config Direct configuration list
#'
#' @return Named list with resolved configuration values
#' @keywords internal
read_config_from_sources <- function(args, config_file = NULL, config = NULL) {
  # Initialize with default values
  result <- list(
    host = NULL,
    port = NULL,
    username = NULL,
    password = NULL,
    ssl_root_cert = NULL,
    ssl_cert = NULL,
    ssl_key = NULL,
    ssl_mode = "verify-ca"
  )
  
  # 1. Start with system environment variables (lowest precedence)
  if (Sys.getenv("OJO_HOST") != "") result$host <- Sys.getenv("OJO_HOST")
  if (Sys.getenv("OJO_PORT") != "") result$port <- Sys.getenv("OJO_PORT")
  if (Sys.getenv("OJO_DEFAULT_USER") != "") result$username <- Sys.getenv("OJO_DEFAULT_USER")
  if (Sys.getenv("OJO_DEFAULT_PASS") != "") result$password <- Sys.getenv("OJO_DEFAULT_PASS")
  if (Sys.getenv("OJO_ADMIN_USER") != "") result$username <- Sys.getenv("OJO_ADMIN_USER")
  if (Sys.getenv("OJO_ADMIN_PASS") != "") result$password <- Sys.getenv("OJO_ADMIN_PASS")
  if (Sys.getenv("OJO_SSL_ROOT_CERT") != "") result$ssl_root_cert <- Sys.getenv("OJO_SSL_ROOT_CERT")
  if (Sys.getenv("OJO_SSL_CERT") != "") result$ssl_cert <- Sys.getenv("OJO_SSL_CERT")
  if (Sys.getenv("OJO_SSL_KEY") != "") result$ssl_key <- Sys.getenv("OJO_SSL_KEY")
  if (Sys.getenv("OJO_SSL_MODE") != "") result$ssl_mode <- Sys.getenv("OJO_SSL_MODE")
  
  # 2. Override with .Renviron values (if different from system env)
  home <- Sys.getenv("HOME")
  renv_path <- fs::path(home, ".Renviron")
  if (fs::file_exists(renv_path)) {
    # Read .Renviron variables (this is already handled by step 1 since Sys.getenv reads .Renviron)
  }
  
  # 3. Override with config file values
  if (!is.null(config_file) && fs::file_exists(config_file)) {
    if (requireNamespace("yaml", quietly = TRUE)) {
      file_config <- yaml::read_yaml(config_file)
      if (!is.null(file_config$ojo)) file_config <- file_config$ojo  # Support nested config
      for (name in names(result)) {
        if (!is.null(file_config[[name]])) {
          result[[name]] <- file_config[[name]]
        }
      }
    } else {
      warning("yaml package not available. Cannot read config file. Install with install.packages('yaml')")
    }
  }
  
  # 4. Override with direct config values
  if (!is.null(config)) {
    for (name in names(result)) {
      if (!is.null(config[[name]])) {
        result[[name]] <- config[[name]]
      }
    }
  }
  
  # 5. Override with function arguments (highest precedence)
  for (name in names(result)) {
    if (!is.null(args[[name]])) {
      result[[name]] <- args[[name]]
    }
  }
  
  return(result)
}

#' @title Resolve SSL certificate paths
#'
#' @description Resolves SSL certificate file paths with fallbacks
#'
#' @param ssl_root_cert Root certificate path
#' @param ssl_cert Client certificate path  
#' @param ssl_key Client key path
#' @param home Home directory path
#'
#' @return Named list with resolved SSL certificate paths
#' @keywords internal
resolve_ssl_paths <- function(ssl_root_cert = NULL, ssl_cert = NULL, ssl_key = NULL, home = NULL) {
  if (is.null(home)) home <- Sys.getenv("HOME")
  
  # Default SSL cert locations
  default_ssl_dir <- fs::path(home, ".postgresql", "ojodb")
  
  result <- list(
    ssl_root_cert = if (!is.null(ssl_root_cert)) ssl_root_cert else fs::path(default_ssl_dir, "server-ca.pem"),
    ssl_cert = if (!is.null(ssl_cert)) ssl_cert else fs::path(default_ssl_dir, "client-cert.pem"),
    ssl_key = if (!is.null(ssl_key)) ssl_key else fs::path(default_ssl_dir, "client-key.pem")
  )
  
  return(result)
}

#' @title Validate SSL certificates
#'
#' @description Validates that SSL certificate files exist and are readable
#'
#' @param ssl_paths Named list of SSL certificate paths
#' @param strict If TRUE, throws error when certs are missing. If FALSE, returns validation status.
#'
#' @return Logical indicating if all certificates are valid (when strict=FALSE)
#' @keywords internal
validate_ssl_certs <- function(ssl_paths, strict = TRUE) {
  missing_certs <- c()
  
  for (cert_type in names(ssl_paths)) {
    cert_path <- ssl_paths[[cert_type]]
    if (!fs::file_exists(cert_path)) {
      missing_certs <- c(missing_certs, paste0(cert_type, " (", cert_path, ")"))
    }
  }
  
  if (length(missing_certs) > 0) {
    if (strict) {
      rlang::abort(
        paste0(
          "SSL certificate files not found:\n",
          paste(missing_certs, collapse = "\n"),
          "\n\nPlease ensure all SSL certificates are available or specify custom paths using the ssl_root_cert, ssl_cert, and ssl_key parameters."
        )
      )
    } else {
      return(FALSE)
    }
  }
  
  return(TRUE)
}

#' @title Create configuration for OJO database connection
#'
#' @description Configure credentials for the Open Justice Oklahoma database
#'
#' @details
#' Assists the user in populating a .Renviron file with the necessary environment variables to connect to the Open Justice Oklahoma database.
#' Can read configuration from multiple sources: function arguments, config files, .Renviron, and system environment variables.
#'
#' @param host The host name of the database server
#' @param port The port number of the database server
#' @param username The username to use to connect to the database
#' @param password The password to use to connect to the database
#' @param ssl_root_cert Path to SSL root certificate file. If NULL, uses default location.
#' @param ssl_cert Path to SSL client certificate file. If NULL, uses default location.
#' @param ssl_key Path to SSL client key file. If NULL, uses default location.
#' @param ssl_mode SSL mode for connection. Default is "verify-ca".
#' @param config_file Path to YAML configuration file containing database settings.
#' @param config A named list containing configuration values.
#' @param ... Placeholder for additional arguments
#' @param .admin A logical value indicating whether to connect to the database as an administrator
#' @param .overwrite A logical value indicating whether to overwrite the existing .Renviron file
#' @param .install A logical value indicating whether to install the database connection or use it only for the current session
#'
#' @export
#' @returns Nothing
#'
#' @examples
#' \dontrun{
#' # Basic usage with all parameters
#' ojo_auth(host = "myhost", port = "5432", username = "user", password = "pass")
#' 
#' # Using custom SSL certificate paths
#' ojo_auth(host = "myhost", port = "5432", username = "user", password = "pass",
#'          ssl_root_cert = "/path/to/server-ca.pem",
#'          ssl_cert = "/path/to/client-cert.pem", 
#'          ssl_key = "/path/to/client-key.pem")
#' 
#' # Using a config file
#' ojo_auth(config_file = "~/.ojo_config.yaml")
#' 
#' # Using a config list
#' config <- list(host = "myhost", port = "5432", username = "user", password = "pass")
#' ojo_auth(config = config)
#' 
#' # Temporary session setup (no .Renviron modification)
#' ojo_auth(host = "myhost", port = "5432", username = "user", password = "pass", .install = FALSE)
#' }
#' @section Side Effects:
#' This function configures database authentication credentials from multiple sources with the following precedence (highest to lowest):
#' 1. Function arguments
#' 2. Direct config list parameter
#' 3. YAML configuration file
#' 4. .Renviron file variables  
#' 5. System environment variables
#'
#' When .install = TRUE (default), credentials are stored in the user's .Renviron file.
#' If the .Renviron file already exists, it will be backed up before modification.
#' When .install = FALSE, credentials are set only for the current R session.
#'
ojo_auth <- function(host = NULL, port = NULL, username = NULL, password = NULL, 
                    ssl_root_cert = NULL, ssl_cert = NULL, ssl_key = NULL, ssl_mode = "verify-ca",
                    config_file = NULL, config = NULL, ..., 
                    .admin = F, .overwrite = T, .install = T) {
  
  # Collect function arguments for configuration resolution
  func_args <- list(
    host = host,
    port = port, 
    username = username,
    password = password,
    ssl_root_cert = ssl_root_cert,
    ssl_cert = ssl_cert,
    ssl_key = ssl_key,
    ssl_mode = ssl_mode
  )
  
  # Read configuration from multiple sources
  config_values <- read_config_from_sources(func_args, config_file, config)
  
  # Resolve SSL certificate paths
  home <- Sys.getenv("HOME")
  renv <- fs::path(home, ".Renviron")
  ssl_paths <- resolve_ssl_paths(
    config_values$ssl_root_cert, 
    config_values$ssl_cert, 
    config_values$ssl_key, 
    home
  )
  
  # Validate SSL certificates (non-strict to allow graceful handling)
  ssl_valid <- validate_ssl_certs(ssl_paths, strict = FALSE)
  if (!ssl_valid) {
    cli::cli_alert_warning("Some SSL certificate files were not found in default locations.")
    cli::cli_alert_info("Proceeding with specified paths. Connection may fail if certificates are not available.")
  }
  
  # Extract final configuration values
  final_host <- config_values$host
  final_port <- config_values$port
  final_username <- config_values$username
  final_password <- config_values$password
  final_ssl_mode <- config_values$ssl_mode
  
  # Validate required parameters
  if (is.null(final_host) || is.null(final_port) || is.null(final_username) || is.null(final_password)) {
    missing <- c()
    if (is.null(final_host)) missing <- c(missing, "host")
    if (is.null(final_port)) missing <- c(missing, "port")
    if (is.null(final_username)) missing <- c(missing, "username")
    if (is.null(final_password)) missing <- c(missing, "password")
    
    rlang::abort(
      paste0(
        "Missing required configuration parameters: ", paste(missing, collapse = ", "), 
        "\nPlease provide them via function arguments, config file, or environment variables."
      )
    )
  }

  if (.install) {

    # Check if .Renviron exists. If it does, make a backup...
    if (fs::file_exists(renv)) {
      # Backup original .Renviron before doing anything else here.
      fs::file_copy(renv, fs::path(home, ".Renviron_backup"),
                    overwrite = TRUE)
    }

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
    hostconcat <- paste0("OJO_HOST='", final_host, "'")
    portconcat <- paste0("OJO_PORT='", final_port, "'")
    if (.admin) {
      userconcat <- paste0("OJO_ADMIN_USER='", final_username, "'")
      passconcat <- paste0("OJO_ADMIN_PASS='", final_password, "'")
    } else {
      userconcat <- paste0("OJO_DEFAULT_USER='", final_username, "'")
      passconcat <- paste0("OJO_DEFAULT_PASS='", final_password, "'")
    }
    sslmodeconcat <- paste0("OJO_SSL_MODE='", final_ssl_mode, "'")
    rootcertconcat <- paste0("OJO_SSL_ROOT_CERT='", ssl_paths$ssl_root_cert, "'")
    clientcertconcat <- paste0("OJO_SSL_CERT='", ssl_paths$ssl_cert, "'")
    clientkeyconcat <- paste0("OJO_SSL_KEY='", ssl_paths$ssl_key, "'")
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
    Sys.setenv(OJO_HOST = final_host)
    Sys.setenv(OJO_PORT = final_port)
    if (.admin == T) {
      Sys.setenv(OJO_ADMIN_USER = final_username)
      Sys.setenv(OJO_ADMIN_PASS = final_password)
    } else {
      Sys.setenv(OJO_DEFAULT_USER = final_username)
      Sys.setenv(OJO_DEFAULT_PASS = final_password)
    }
    Sys.setenv(OJO_SSL_MODE = final_ssl_mode)
    Sys.setenv(OJO_SSL_ROOT_CERT = ssl_paths$ssl_root_cert)
    Sys.setenv(OJO_SSL_CERT = ssl_paths$ssl_cert)
    Sys.setenv(OJO_SSL_KEY = ssl_paths$ssl_key)
  }
  invisible()
}
