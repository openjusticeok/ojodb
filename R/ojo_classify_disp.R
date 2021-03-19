#' Classify hand-entered charges in court and jail data
#'
#' Uses string detection to standardize dirty administrative records of dispositions.
#'
#' Run `ojo_ct_list` to see the list of charges currently supported by the `ojo_classify_ct()` function.
#'
#' @param ct_desc A string that contains a charge description
#' @return The standardized charge
#' @examples
#'
#' ojo_classify_ct("SECOND DEGREE BURGLARY")
#'
#' ojo_ct_list

ojo_evgranted <- function(disp) {
  str_detect(disp, "JUDG") & !str_detect(disp, "DEFEN")
}




