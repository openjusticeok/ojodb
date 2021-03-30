#' Create SMTP credentials file for ojo.alerts email address
#'
#' Creates a credentials file called "sib" in the user's home directory to be used for automated email distribution. Requires a master password to be entered upon creation.
#'
#' @examples
#' \dontrun{
#' ojo_alert_creds()
#' }

ojo_alert_creds <- function() {
  blastula::create_smtp_creds_file(file = "~/sib",
                        user = "ojo.alerts@gmail.com",
                        host = "smtp-relay.sendinblue.com",
                        port = 587,
                        use_ssl = FALSE)
}
