#' Classify hand-entered charges in court and jail data
#'
#' Uses string detection to standardize dirty administrative records of criminal offenses into analyzable form. This is a work in progress, and only includes a few charges right now: burglary, PWID, and drug trafficking. As we continue to work on new charges, it will be updated with new definitions.
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

ojo_classify_ct <- function(ct_desc) {
  case_when(str_detect(ct_desc, "BURG") ~
              case_when(str_detect(ct_desc, "FIRST|1ST") ~ "BURGLARY I",
                        str_detect(ct_desc, "SECOND|2ND") ~ "BURGLARY II",
                        str_detect(ct_desc, "THIRD|3RD") ~ "BURGLARY III",
                        str_detect(ct_desc, "TOOL") ~ "POSS BURGLARY TOOLS",
                        TRUE ~ "BURGLARY (UNSPECIFIED)"),
            str_detect(ct_desc,"INTENT|DISTR") &
              !str_detect(ct_desc, "MISDEMEANOR|HUMAN|VEHICLE|FIREARM|CULTIVATION|MURDER|PORN|STEAL|SHOOT|ASSAULT|BREAK") ~ "PWID",
            str_detect(ct_desc,"TRAFFICK") &
              !str_detect(ct_desc, "HUMAN|CHILD|SEX") ~ "DRUG TRAFFICKING",
            (str_detect(ct_desc, "LARC") & str_detect(ct_desc, "RETAIL")) |
              str_detect(ct_desc, "LMFR") ~ "LARCENY OF MERCHANDISE FROM A RETAILER",
            TRUE ~ "UNKNOWN"
  )
}

ojo_ct_list <- c("BURGLARY I",
                 "BURGLARY II",
                 "BURGLARY III",
                 "POSS BURGLARY TOOLS",
                 "BURGLARY (UNSPECIFIED)",
                 "PWID",
                 "DRUG TRAFFICKING",
                 "LARCENY OF MERCHANDISE FROM A RETAILER")


