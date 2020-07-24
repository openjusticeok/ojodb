library(rvest)
library(tidyverse)
library(lubridate)
library(RMySQL)
library(dbx)
library(parallel)
library(glue)

### Load reference tables ####
casetype_codes <- read_csv("data/casetype-codes.csv")
court_ref <- read_csv("data/court-reference.csv")

fbi_key <- "dbx8mcUynYUfRlsyschgg9VSBMD7yO334g8pbQfx"

all_courts <- court_ref$court
odcr_counties <- court_ref[court_ref$site == "ODCR", "court"] %>% as.list %>% unlist
oscn_counties <- court_ref[court_ref$site == "OSCN", "court"] %>% as.list %>% unlist

courtlist <- as.list(court_ref$court_code)
names(courtlist) <- as.list(court_ref$court)

tables <- c("events", "caseinfo", "crim_disps", "civ_disps",
            "mins", "atts", "lastcase", "citations", "partynames", "pprofile",
            "party", "party_address", "updates", "lastupdate",
            "odcr_caseinfo", "odcr_events", "odcr_parties", "odcr_mins", "odcr_party_profile", "odcr_pays",
            "odcr_disps", "odcr_updates", "odcr_lastupdate")

ojo_example <- read_csv("data/ojo_example.csv")

#### Data manipulation helper functions ####
cat_violent <- function(x, definition = "571"){
  str_detect(x, "EXPLO| KILL|MURDER|RAPE|MANSLAU|MAIM|KIDNAP|ROBB|ABUSE|LEWD|INDECENT|SODOM|FIREARM|RIOT|INJUR|ARSON|SABOTA|EXTOR|BOMB| PORN|CHILD PROS") |
    (str_detect(x, "TRAFF") & str_detect(x, "CHILD|MINOR")) |
    (str_detect(x, "FIREARM|GUN|WEAPON") & str_detect(x, "POINT|FELONY")) |
    (str_detect(x, "BURG") & str_detect(x, "FIRST|1")) |
    (str_detect(x, "ASSAULT|BATTERY|A&B|A & B") & !str_detect(x, "DOMESTIC"))
}


