#' @title Create database configuration object
#'
#' @description Creates a configuration object for OJO database connections with flexible configuration sources.
#'
#' @param host Database host name
#' @param port Database port number  
#' @param username Database username
#' @param password Database password
#' @param ssl_root_cert Path to SSL root certificate file
#' @param ssl_cert Path to SSL client certificate file
#' @param ssl_key Path to SSL private key file
#' @param ssl_mode SSL connection mode (default: "verify-ca")
#' @param config_file Path to YAML configuration file
#' @param .admin Use admin credentials from environment variables
#'
#' @details
#' This function creates a database configuration object by reading from multiple sources 
#' with the following precedence (highest to lowest):
#' 1. Function arguments
#' 2. YAML configuration file
#' 3. Environment variables (.Renviron and system)
#'
#' @return A db_config object (list) containing database connection parameters
#' @export
#'
#' @examples
#' \dontrun{
#' # Create config with explicit parameters
#' config <- db_config(
#'   host = "localhost", 
#'   port = "5432",
#'   username = "user", 
#'   password = "pass"
#' )
#' 
#' # Create config from YAML file
#' config <- db_config(config_file = "~/.ojo_config.yaml")
#' 
#' # Create config using admin credentials from environment
#' config <- db_config(.admin = TRUE)
#' }
db_config <- function(host = NULL, port = NULL, username = NULL, password = NULL,
                     ssl_root_cert = NULL, ssl_cert = NULL, ssl_key = NULL, 
                     ssl_mode = "verify-ca", config_file = NULL, .admin = FALSE) {
  
  # Initialize default configuration
  config <- list(
    host = NULL,
    port = NULL,
    username = NULL,
    password = NULL,
    ssl_root_cert = NULL,
    ssl_cert = NULL,
    ssl_key = NULL,
    ssl_mode = "verify-ca"
  )
  
  # Read from environment variables first
  if (Sys.getenv("OJO_HOST") != "") config$host <- Sys.getenv("OJO_HOST")
  if (Sys.getenv("OJO_PORT") != "") config$port <- Sys.getenv("OJO_PORT")
  
  if (.admin) {
    if (Sys.getenv("OJO_ADMIN_USER") != "") config$username <- Sys.getenv("OJO_ADMIN_USER")
    if (Sys.getenv("OJO_ADMIN_PASS") != "") config$password <- Sys.getenv("OJO_ADMIN_PASS")
  } else {
    if (Sys.getenv("OJO_DEFAULT_USER") != "") config$username <- Sys.getenv("OJO_DEFAULT_USER") 
    if (Sys.getenv("OJO_DEFAULT_PASS") != "") config$password <- Sys.getenv("OJO_DEFAULT_PASS")
  }
  
  if (Sys.getenv("OJO_SSL_ROOT_CERT") != "") config$ssl_root_cert <- Sys.getenv("OJO_SSL_ROOT_CERT")
  if (Sys.getenv("OJO_SSL_CERT") != "") config$ssl_cert <- Sys.getenv("OJO_SSL_CERT")
  if (Sys.getenv("OJO_SSL_KEY") != "") config$ssl_key <- Sys.getenv("OJO_SSL_KEY")
  if (Sys.getenv("OJO_SSL_MODE") != "") config$ssl_mode <- Sys.getenv("OJO_SSL_MODE")
  
  # Override with config file if provided
  if (!is.null(config_file) && file.exists(config_file)) {
    if (requireNamespace("yaml", quietly = TRUE)) {
      file_config <- yaml::read_yaml(config_file)
      # Support both flat and nested config structures
      if (!is.null(file_config$ojo)) file_config <- file_config$ojo
      
      for (name in names(config)) {
        if (!is.null(file_config[[name]])) {
          config[[name]] <- file_config[[name]]
        }
      }
    } else {
      warning("yaml package not available. Install with install.packages('yaml')")
    }
  }
  
  # Override with function arguments (highest precedence)
  if (!is.null(host)) config$host <- host
  if (!is.null(port)) config$port <- port
  if (!is.null(username)) config$username <- username
  if (!is.null(password)) config$password <- password
  if (!is.null(ssl_root_cert)) config$ssl_root_cert <- ssl_root_cert
  if (!is.null(ssl_cert)) config$ssl_cert <- ssl_cert
  if (!is.null(ssl_key)) config$ssl_key <- ssl_key
  if (!is.null(ssl_mode)) config$ssl_mode <- ssl_mode
  
  # Set default SSL certificate paths if not specified
  home <- Sys.getenv("HOME")
  default_ssl_dir <- file.path(home, ".postgresql", "ojodb")
  
  if (is.null(config$ssl_root_cert)) {
    config$ssl_root_cert <- file.path(default_ssl_dir, "server-ca.pem")
  }
  if (is.null(config$ssl_cert)) {
    config$ssl_cert <- file.path(default_ssl_dir, "client-cert.pem")
  }
  if (is.null(config$ssl_key)) {
    config$ssl_key <- file.path(default_ssl_dir, "client-key.pem")
  }
  
  # Add class for method dispatch
  class(config) <- c("db_config", "list")
  config
}

#' @title Configure OJO database authentication
#'
#' @description Sets up authentication for the Open Justice Oklahoma database using a db_config object.
#'
#' @param db_config A db_config object created by `db_config()`
#' @param .overwrite Logical indicating whether to overwrite existing .Renviron entries
#' @param .install Logical indicating whether to save configuration to .Renviron (TRUE) or use for current session only (FALSE)
#'
#' @details
#' This function takes a db_config object and sets up the database authentication.
#' When .install = TRUE (default), credentials are saved to .Renviron for persistent use.
#' When .install = FALSE, credentials are only set for the current R session.
#'
#' @return Invisible NULL
#' @export
#'
#' @examples
#' \dontrun{
#' # Create config and authenticate  
#' config <- db_config(host = "localhost", port = "5432", username = "user", password = "pass")
#' ojo_auth(config)
#' 
#' # One-liner with explicit parameters
#' ojo_auth(db_config(host = "localhost", port = "5432", username = "user", password = "pass"))
#' 
#' # From config file
#' ojo_auth(db_config(config_file = "~/.ojo_config.yaml"))
#' 
#' # Temporary session only
#' ojo_auth(db_config(host = "localhost", port = "5432", username = "user", password = "pass"), .install = FALSE)
#' }
#'
ojo_auth <- function(db_config, .overwrite = TRUE, .install = TRUE) {
  
  # Validate input
  if (!inherits(db_config, "db_config")) {
    stop("db_config must be a db_config object created by db_config()", call. = FALSE)
  }
  
  # Check required parameters
  required <- c("host", "port", "username", "password")
  missing <- required[sapply(required, function(x) is.null(db_config[[x]]))]
  if (length(missing) > 0) {
    stop("Missing required configuration: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  
  # Validate SSL certificates exist if using verify modes
  if (db_config$ssl_mode %in% c("verify-ca", "verify-full")) {
    missing_certs <- c()
    ssl_files <- c("ssl_root_cert", "ssl_cert", "ssl_key")
    for (cert_type in ssl_files) {
      cert_path <- db_config[[cert_type]]
      if (!is.null(cert_path) && !file.exists(cert_path)) {
        missing_certs <- c(missing_certs, paste0(cert_type, " (", cert_path, ")"))
      }
    }
    
    if (length(missing_certs) > 0) {
      warning("SSL certificate files not found:\n", paste(missing_certs, collapse = "\n"),
              "\nConnection may fail. Consider using ssl_mode = 'require' or providing valid certificate paths.",
              call. = FALSE)
    }
  }
  
  # Determine if this is admin configuration based on username pattern
  is_admin <- !is.null(db_config$username) && grepl("admin", tolower(db_config$username))
  
  if (.install) {
    home <- Sys.getenv("HOME")
    renv <- file.path(home, ".Renviron")
    
    # Backup existing .Renviron
    if (file.exists(renv)) {
      file.copy(renv, file.path(home, ".Renviron_backup"), overwrite = TRUE)
    } else {
      file.create(renv)
    }
    
    # Clean existing OJO variables if overwriting
    if (.overwrite && file.exists(renv)) {
      lines <- readLines(renv)
      # Remove all OJO-related lines
      clean_lines <- lines[!grepl("^OJO_", lines)]
      
      # Write new configuration
      new_vars <- c(
        clean_lines,
        paste0("OJO_HOST='", db_config$host, "'"),
        paste0("OJO_PORT='", db_config$port, "'"),
        if (is_admin) paste0("OJO_ADMIN_USER='", db_config$username, "'") else paste0("OJO_DEFAULT_USER='", db_config$username, "'"),
        if (is_admin) paste0("OJO_ADMIN_PASS='", db_config$password, "'") else paste0("OJO_DEFAULT_PASS='", db_config$password, "'"),
        paste0("OJO_SSL_MODE='", db_config$ssl_mode, "'"),
        paste0("OJO_SSL_ROOT_CERT='", db_config$ssl_root_cert, "'"),
        paste0("OJO_SSL_CERT='", db_config$ssl_cert, "'"),
        paste0("OJO_SSL_KEY='", db_config$ssl_key, "'")
      )
      writeLines(new_vars, renv)
    } else if (!.overwrite && file.exists(renv)) {
      # Check for conflicts
      lines <- readLines(renv)
      user_type <- if (is_admin) "OJO_ADMIN_USER" else "OJO_DEFAULT_USER"
      if (any(grepl(user_type, lines))) {
        stop("Configuration already exists. Use .overwrite = TRUE to replace it.", call. = FALSE)
      }
      
      # Append new configuration
      new_vars <- c(
        paste0("OJO_HOST='", db_config$host, "'"),
        paste0("OJO_PORT='", db_config$port, "'"),
        if (is_admin) paste0("OJO_ADMIN_USER='", db_config$username, "'") else paste0("OJO_DEFAULT_USER='", db_config$username, "'"),
        if (is_admin) paste0("OJO_ADMIN_PASS='", db_config$password, "'") else paste0("OJO_DEFAULT_PASS='", db_config$password, "'"),
        paste0("OJO_SSL_MODE='", db_config$ssl_mode, "'"),
        paste0("OJO_SSL_ROOT_CERT='", db_config$ssl_root_cert, "'"),
        paste0("OJO_SSL_CERT='", db_config$ssl_cert, "'"),
        paste0("OJO_SSL_KEY='", db_config$ssl_key, "'")
      )
      cat(new_vars, file = renv, sep = "\n", append = TRUE)
    } else {
      # New .Renviron file
      new_vars <- c(
        paste0("OJO_HOST='", db_config$host, "'"),
        paste0("OJO_PORT='", db_config$port, "'"),
        if (is_admin) paste0("OJO_ADMIN_USER='", db_config$username, "'") else paste0("OJO_DEFAULT_USER='", db_config$username, "'"),
        if (is_admin) paste0("OJO_ADMIN_PASS='", db_config$password, "'") else paste0("OJO_DEFAULT_PASS='", db_config$password, "'"),
        paste0("OJO_SSL_MODE='", db_config$ssl_mode, "'"),
        paste0("OJO_SSL_ROOT_CERT='", db_config$ssl_root_cert, "'"),
        paste0("OJO_SSL_CERT='", db_config$ssl_cert, "'"),
        paste0("OJO_SSL_KEY='", db_config$ssl_key, "'")
      )
      writeLines(new_vars, renv)
    }
    cat("Configuration saved to .Renviron\n")
    cat("To use now, restart R or run readRenviron('~/.Renviron')\n")
    
  } else {
    # Set environment variables for current session only
    Sys.setenv(OJO_HOST = db_config$host)
    Sys.setenv(OJO_PORT = db_config$port)
    
    if (is_admin) {
      Sys.setenv(OJO_ADMIN_USER = db_config$username)
      Sys.setenv(OJO_ADMIN_PASS = db_config$password)
    } else {
      Sys.setenv(OJO_DEFAULT_USER = db_config$username)
      Sys.setenv(OJO_DEFAULT_PASS = db_config$password)
    }
    
    Sys.setenv(OJO_SSL_MODE = db_config$ssl_mode)
    Sys.setenv(OJO_SSL_ROOT_CERT = db_config$ssl_root_cert)
    Sys.setenv(OJO_SSL_CERT = db_config$ssl_cert)
    Sys.setenv(OJO_SSL_KEY = db_config$ssl_key)
    
    cat("Configuration set for current session only\n")
    cat("To persist configuration, run with .install = TRUE\n")
  }
  
  invisible()
}
