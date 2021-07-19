#' Browse the source court record of a row
#'
#' Displays the OSCN court record for a given row in a dataframe with columns named `court` and `casenum`
#'
#'@param df A local dataframe with columns named `court` and `casenum`
#'@param row_number The number of the row of the case to be shown
#'
#' @examples
#' 

show_row <- function(df, row_number) {
  if ("court" %in% names(df) & "casenum" %in% names(df) & row_number <= nrow(df)) {
    paste0("https://www.oscn.net/dockets/GetCaseInformation.aspx?db=",
           df[row_number, ]$court,
           "&number=",
           df[row_number, ]$casenum) %>% 
      browseURL()
  } else {
    message("Data frame must contain columns `court` and `casenum`.")
  }
}
