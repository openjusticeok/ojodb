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

ojo_dir <- getwd()

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

#### Database connection functions #####

oscn_reset <- function() {
  objs <- ls(pos = ".GlobalEnv")
  rm(list = objs[objs %in% tables], pos = ".GlobalEnv")
}

#### OSCN scraping functions ####

oscn_why_not <- function(courts, casetypes, years, cores = 8) {
  mclapply(courts, function(x) oscn_scrape_all(x, casetypes, years), mc.cores = cores)
}

#### ODCR scraping functions ####
odcr_show <- function(court, casetype, year, caseseq) {
  u <- court_ref[court_ref$court == court, "url_pattern"] %>%
    str_replace("XX", paste0(casetype, "+")) %>%
    str_replace("YY", str_sub(year, 3, 4)) %>%
    str_replace("ZZZZ", str_pad(caseseq, side = "left", pad = 0, width = 4))
  browseURL(u)
}

odcr_updatedb <- function() {

  connect_ojo()

  if (exists("odcr_caseinfo")) { dbWriteTable(ojo_db, "odcr_caseinfo", odcr_caseinfo, row.names = FALSE, append = TRUE) }
  if (exists("odcr_events")) {dbWriteTable(ojo_db, "odcr_events", odcr_events, row.names = FALSE, append = TRUE)}
  if (exists("odcr_parties")) { dbWriteTable(ojo_db, "odcr_party", odcr_parties, row.names = FALSE, append = TRUE)}
  if (exists("odcr_party_profile")) { dbWriteTable(ojo_db, "odcr_party_profile", odcr_party_profile, row.names = FALSE, append = TRUE)}
  if (exists("odcr_updates")) { dbWriteTable(ojo_db, "odcr_updates", odcr_updates, row.names = FALSE, append = TRUE)}
  if (exists("odcr_lastupdate")) { dbxUpsert(ojo_db, "odcr_lastupdate", as.data.frame(odcr_lastupdate), where_cols = c("odcr_lastupdate_id"))}
  if (exists("odcr_disps")) { dbWriteTable(ojo_db, "odcr_disps", odcr_disps, row.names = FALSE, append = TRUE)}

  if (exists("odcr_mins")) {
    years <- unique(odcr_mins$file_year)
    casetypes <- unique(odcr_mins$casetype)

    for (i in years) {
      for (j in casetypes) {
        if (!paste0("odcr_mins_", i, str_to_upper(j)) %in% dbListTables(ojo_db)) {
          dbGetQuery(ojo_db,
                     paste0(
                       "CREATE TABLE `", paste0("odcr_mins_", i, str_to_upper(j)),
                       "` (`odcr_min_id` varchar(50) NOT NULL,
          `court` varchar(50) DEFAULT NULL,
          `casenum` varchar(50) DEFAULT NULL,
          `casetype` varchar(6) DEFAULT NULL,
          `file_year` int(6) DEFAULT NULL,
          `min_date` date DEFAULT NULL,
          `min_desc` text,
          `fee_amt` double DEFAULT NULL,
          PRIMARY KEY (`odcr_min_id`),
          KEY `odcr_min_id` (`odcr_min_id`),
          KEY `court` (`court`),
          KEY `casenum` (`casenum`),
          KEY `casetype` (`casetype`),
          KEY `file_year` (`file_year`),
          FULLTEXT KEY `min_desc` (`min_desc`)
          ) ENGINE=InnoDB DEFAULT CHARSET=utf8;"
                     )
          )
        }

        dbWriteTable(ojo_db,
                     paste0("odcr_mins_", i, str_to_upper(j)),
                     odcr_mins[odcr_mins$file_year == i & odcr_mins$casetype == j, ],
                     row.names = FALSE, append = TRUE)

        if (!paste0("odcr_pays_", i) %in% dbListTables(ojo_db)) {
          dbGetQuery(ojo_db, paste0(
            "CREATE TABLE `odcr_pays_", i, "` (
  `odcr_pay_id` varchar(50) NOT NULL,
`court` varchar(50) DEFAULT NULL,
`casenum` varchar(50) DEFAULT NULL,
`casetype` varchar(10) DEFAULT NULL,
`file_year` int(6) DEFAULT NULL,
`pay_date` date DEFAULT NULL,
`pay_desc` varchar(50) DEFAULT NULL,
`pay_amt` double DEFAULT NULL,
PRIMARY KEY (`odcr_pay_id`),
KEY `court` (`court`),
KEY `casenum` (`casenum`),
KEY `casetype` (`casetype`),
KEY `file_year` (`file_year`),
KEY `pay_date` (`pay_date`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
"
          ))
        }

        if (exists("odcr_pays")) {
          dbWriteTable(ojo_db,
                       paste0("odcr_pays_", i),
                       odcr_pays[odcr_pays$file_year == i,],
                       row.names = FALSE, append = TRUE)
        }

      }
    }
  }
  if (exists("odcr_updates")) { dbWriteTable(ojo_db, "odcr_updates", odcr_updates,
                                             append = TRUE, row.names = FALSE) }

  dbDisconnect(ojo_db)

  print("Database updated.")
}

odcr_why_not <- function(courts, casetypes, years, cores = 8) {
  mclapply(courts, function(x) odcr_scrape_all(x, casetypes, years), mc.cores = cores)
}

oscn_scrape_dates <- function(courts, casetypes, from_mdy, to_mdy, updatedb = FALSE) {
  ctlist <- casetype_codes %>%
    filter(casetype %in% casetypes)

  from_m <- str_extract(from_mdy, "\\d{1,2}(?=(/|-))")
  from_d <- str_extract(from_mdy, "(?<=(/|-))\\d{1,2}(?=(/|-))")
  from_y <- str_extract(from_mdy, "(?<=(/|-))\\d{4}$")

  to_m <- str_extract(to_mdy, "\\d{1,2}(?=(/|-))")
  to_d <- str_extract(to_mdy, "(?<=(/|-))\\d{1,2}(?=(/|-))")
  to_y <- str_extract(to_mdy, "(?<=(/|-))\\d{4}$")

  for (l in courts) {
    court_tmp <- l

    for (k in ctlist$casetype_id) {
      casetype_id <- k

      d <- read_html(paste0("http://www.oscn.net/dockets/Results.aspx?db=",
                            str_remove_all(court_tmp, " "),
                            "&number=&lname=&fname=&mname=&DoBMin=&DoBMax=&partytype=&apct=&dcct=",
                            casetype_id,
                            "&FiledDateL=",
                            from_m, "%2F", from_d, "%2F", from_y,
                            "&FiledDateH=&ClosedDateL=&ClosedDateH=&iLC=&iLCType=&iYear=&iNumber=&citation="))

      if (d %>% html_node("body") %>%
          html_text() %>%
          str_detect("Found No Records") == FALSE) {

        d <- d %>%
          html_node("table") %>%
          html_table()

        d$seq <- str_extract(d$`Case Number`, "(?<=-)\\d{1,5}($|A)") %>% str_remove_all("[[:alpha:]]") %>% as.numeric()
        d$casetype <- str_extract(d$`Case Number`, "\\w{2}(?=-)")

        from_seq <- min(d$seq)
      }

      d <- read_html(paste0("http://www.oscn.net/dockets/Results.aspx?db=",
                            str_remove_all(court_tmp, " "),
                            "&number=&lname=&fname=&mname=&DoBMin=&DoBMax=&partytype=&apct=&dcct=",
                            casetype_id,
                            "&FiledDateL=",
                            to_m, "%2F", to_d, "%2F", to_y,
                            "&FiledDateH=&ClosedDateL=&ClosedDateH=&iLC=&iLCType=&iYear=&iNumber=&citation="))

      if (d %>% html_node("body") %>%
          html_text() %>%
          str_detect("Found No Records") == FALSE) {

        d <- d %>%
          html_node("table") %>%
          html_table()

        d$seq <- str_extract(d$`Case Number`, "(?<=-)\\d{1,5}($|A)") %>% str_remove_all("[[:alpha:]]") %>% as.numeric()
        d$casetype <- str_extract(d$`Case Number`, "\\w{2}(?=-)")

        to_seq <- max(d$seq)
      }

      oscn_scrape(courts = court_tmp, casetype = as.character(ctlist[ctlist$casetype_id == casetype_id, 1]),
                  years = as.numeric(from_y), case_seqs = from_seq:to_seq, updatedb = FALSE)
    }
  }
}

oscn_scrape_parties <- function(courts, casetypes, years, case_seqs, updatedb = TRUE) {

  for (l in courts) {
    court_tmp <- l

    for (m in casetypes) {
      casetype_tmp <- m

      for (j in years) {
        caseyear_tmp <- j

        for (k in case_seqs) {

          start <- Sys.time()
          caseseq_tmp <- k
          casenum_tmp <- paste(casetype_tmp, caseyear_tmp, caseseq_tmp, sep = "-")

          l <- read_html(paste0("http://www.oscn.net/dockets/GetCaseInformation.aspx?db=",
                                court_tmp, "&number=", casenum_tmp)) %>%
            html_nodes("p a") %>%
            html_attrs() %>%
            unlist

          l <- l[str_detect(l, "GetParty") & !str_detect(l, "id=32000$")] %>%
            as.tibble

          if (nrow(l) == 0) {} else {
            ### Loop through party links

            for (k in 1:nrow(l)) {

              p <- read_html(paste0("http://www.oscn.net/dockets/", l[k, 1])) %>%
                html_nodes("table") %>%
                html_table

              def_id_tmp <- str_extract(l[k, 1], "(?<=id=).*")

              for (i in 1:length(p)) {
                if ("Requested Party" %in% names(p[[i]])) {
                  party_tmp <- p[[i]] %>%
                    mutate(defname = `Requested Party` %>%
                             str_to_upper,
                           alias = `Alias or Alternate Names` %>%
                             str_to_upper) %>%
                    mutate(def_id = def_id_tmp) %>%
                    mutate(court = str_to_upper(court_tmp),
                           casenum = casenum_tmp %>%
                             str_to_upper) %>%
                    select(court, casenum, def_id, defname, def_alias = alias)
                  if (exists("party")) {party <<- bind_rows(party, party_tmp)
                  } else {
                    party <<- party_tmp
                  }
                } else if ("Marital Status" %in% names(p[[i]])) {
                  profile_tmp <- p[[i]] %>%
                    mutate(rec_date = mdy(`Record Date`),
                           def_mob = paste0("1/", `Birth Month and Year`) %>% dmy,
                           def_id = def_id_tmp) %>%
                    mutate(court = str_to_upper(court_tmp),
                           casenum = casenum_tmp %>%
                             str_to_upper) %>%
                    select(court, casenum, def_id, rec_date, def_mob)
                  if (exists("pprofile")) {pprofile <<- bind_rows(pprofile, profile_tmp)
                  } else {
                    pprofile <<- profile_tmp
                  }
                } else if ("Address" %in% names(p[[i]])) {
                  paddr_tmp <- p[[i]] %>%
                    mutate(rec_date = mdy(`Record Date`),
                           def_address = as.character(Address) %>%
                             str_to_upper,
                           def_zip = str_extract(Address, "\\d{5}"),
                           def_id = def_id_tmp,
                           court = str_to_upper(court_tmp),
                           casenum = casenum_tmp %>%
                             str_to_upper) %>%
                    select(court, casenum, def_id, rec_date, def_address, def_zip)
                  if (exists("party_address")) {party_address <<- bind_rows(party_address, paddr_tmp)
                  } else {
                    party_address <<- paddr_tmp }
                }
              }
            }
          }
          print(paste(court_tmp, casenum_tmp, "parties scraped in", Sys.time() - start, "seconds."))
          last_scraped <<- caseseq_tmp
        }
      }
    }
  }
  if (updatedb == TRUE) {
    oscn_updatedb()
    oscn_dedup()
    print("Database updated.")
  }
}

#### Other functions ####
dlm_updatedb <- function() {
  if (exists("dlm_inmates") & exists("dlm_offenses")) {

    dbWriteTable(ojo_db, "dlm_inmates", dlm_inmates,
                 field.types = c(pop_date = "DATE",
                                 refno = "TEXT",
                                 defname = "TEXT",
                                 def_dob = "DATE",
                                 def_race = "TEXT",
                                 def_sex = "TEXT",
                                 address = "TEXT",
                                 address2 = "TEXT",
                                 arrest_off = "TEXT",
                                 arrest_agency = "TEXT",
                                 arrest_date = "DATE",
                                 cell = "TEXT"),
                 append = TRUE, row.names = FALSE)

    dbWriteTable(ojo_db, "dlm_offenses", dlm_offenses,
                 field.types = c(pop_date = "DATE",
                                 refno = "TEXT",
                                 defname = "TEXT",
                                 offense = "TEXT",
                                 casenum = "TEXT",
                                 next_step = "TEXT",
                                 next_date = "DATE",
                                 bond_type = "TEXT",
                                 bond_amt = "DOUBLE"),
                 append = TRUE, row.names = FALSE) }

  else {print('dlm_inmate_update requires data frames called `dlm_inmates` and `dlm_offenses`')}
}

theme_ojo <- function() {
  theme_bw(base_size=14, base_family="Menlo") %+replace%
    theme(
      panel.background  = element_blank(),
      plot.background = element_blank(),
      legend.background = element_rect(fill="transparent", colour=NA),
      legend.key = element_rect(fill="transparent", colour=NA)
    )
}

ojo_pal <- c("#F8D64E", "black", "#0D0887FF", "#6A00A8FF",
             "#B12A90FF", "#E16462FF", "#FCA636FF", "#F0F921FF")

#### Data manipulation helper functions ####
cat_violent <- function(x, definition = "571"){
  str_detect(x, "EXPLO| KILL|MURDER|RAPE|MANSLAU|MAIM|KIDNAP|ROBB|ABUSE|LEWD|INDECENT|SODOM|FIREARM|RIOT|INJUR|ARSON|SABOTA|EXTOR|BOMB| PORN|CHILD PROS") |
    (str_detect(x, "TRAFF") & str_detect(x, "CHILD|MINOR")) |
    (str_detect(x, "FIREARM|GUN|WEAPON") & str_detect(x, "POINT|FELONY")) |
    (str_detect(x, "BURG") & str_detect(x, "FIRST|1")) |
    (str_detect(x, "ASSAULT|BATTERY|A&B|A & B") & !str_detect(x, "DOMESTIC"))
}

date_to_fy <- function(date) {
  ifelse(month(date) > 6, year(date) + 1, year(date))
}
