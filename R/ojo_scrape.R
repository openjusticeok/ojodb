#' Scrape court records from OSCN
#'
#' Scrapes OSCN.net docket records and updates tables in the OJO database.
#'
#' @export ojo_scrape
#' @param districts A character vector of district names
#' @param case_types A character vector of case type abbreviations
#' @param years A numeric vector of years for which to get population data
#' @param case_seqs A numeric vector of case filing sequence numbers
#' @examples
#' \dontrun{
#' # Scrape CF-2022-1 filed in Tulsa County
#' ojo_scrape("TULSA", "CF", 2022, 1)
#' # Scrape the first 10 small claims and traffic cases filed in Garvin and Rogers Counties in 2020 and 2021
#' ojo_scrape(c("GARVIN", "ROGERS"), c("SC", "TR"), 2020:2021, 1:10)
#' }
#' @section Aliases:
#'  For comfort, `ojo_connect` and `connect_ojo` can be used interchangeably.
#' @seealso ojo_scrape_all()

court_ref <- read_csv("inst/extdata/court reference.csv")

parse_page <- function(ht) {
  options(warn = -1)
  start <- Sys.time()

  d <- ht |>
    html_nodes("table") |>
    html_table()

  case_number_tmp <- str_extract(d[[1]]$X2, "(?<=No\\. ).*?(?=\r)") |>
    str_trim()

  district_tmp <- ht |>
    html_nodes("a") |>
    html_text() |>
    pluck(18) |>
    str_remove("County") |>
    str_trim()

  district_tmp <- case_when(district_tmp == "BRISTOW" ~ "CREEK (BRISTOW)",
                            district_tmp == "HENRYETTA" ~ "OKMULGEE (HENRYETTA)",
                            district_tmp == "DRUMRIGHT" ~ "CREEK (DRUMRIGHT)",
                            district_tmp == "PONCA CITY" ~ "KAY (PONCA CITY)",
                            TRUE ~ district_tmp)

  case_type_tmp <- str_sub(case_number_tmp, 1, 2)

  caseyear_tmp <- case_number_tmp |>
    str_sub(4, 7) |>
    as.integer()

  caseseq_tmp <- case_number_tmp |>
    str_remove("[:alpha:]$") |>
    str_extract("\\d{1,5}$")

  case_id_tmp <- paste0('{"district": "',
                        district_tmp,
                        '", "case_number": "',
                        case_number_tmp,
                        '"}')

  #### Skip scraping if case record is empty, record that it's unavailable in oscn_updates ####
  if (length(d) < 2) {

    message(paste("Case information not available for", district_tmp, case_number_tmp))

  } else {
    #### open_count #####
    open_count_tmp <- ht |>
      html_elements("p") |>
      html_text() |>
      enframe() |>
      mutate(open_count = str_squish(value)) |>
      filter(str_detect(open_count, "^\\d{1,2}\\.")) |>
      mutate(id = case_id_tmp) |>
      select(id,
             open_count) |>
      group_by(id) |>
      summarize(open_counts = paste0("{",
                                     paste0('"', open_count, '"',
                                            collapse = ","),
                                     "}"),
                .groups = "keep")

    ### Party names list ####

    pn <- ht |>
      html_nodes("p")

    pn <- pn[2] |>
      as.character() |>
      enframe() |>
      mutate(value = as.character(value) |>
               str_remove_all("<(/|)p>") |>
               str_squish())

    if (str_detect(pn[1, "value"], "href")) {
      parties_tmp <- pn |>
        separate(value, into = paste0("party", 1:20), sep = "<br>",
                 extra = "drop", fill = "right") |>
        pivot_longer(cols = contains("party"), names_to = "pnum", values_to = "text") |>
        filter(text != "", !is.na(text)) |>
        mutate(case_id = case_id_tmp,
               oscn_id = str_extract(text, "(?<=id=)\\d{1,20}"),
               created_at = Sys.time(),
               updated_at = Sys.time(),
               name = str_extract(text, "(?<=\\>).*(?=\\<)") |>
                 str_trim(),
               role = text |>
                 str_extract("(?<=\\>, ).*?$") |>
                 str_trim() |>
                 str_to_sentence() |>
                 str_replace(" ", "_")) |>
        mutate(id = paste0('{"case": ', case_id,
                           ' ,\"name\":\"', oscn_id, '", ',
                           ' ,\"role\":\"', role, '\"}"}')) |>
        select(id,
               case_id,
               oscn_id,
               name,
               role,
               created_at,
               updated_at)

    } else {
      #### party table ####
      parties_tmp <- pn |>
        separate(value, into = paste0("party", 1:20), sep = "<br>",
                 extra = "drop", fill = "right") |>
        pivot_longer(cols = contains("party"), names_to = "pnum", values_to = "party_name") |>
        filter(party_name != "", !is.na(party_name)) |>
        separate(party_name, into = c("party_name", "role"),
                 sep = ", (?=[[:alpha:]]*$)") |>
        mutate(case_id = case_id_tmp,
               created_at = Sys.time(),
               updated_at = Sys.time(),
               name = party_name,
               role = role |>
                 str_trim() |>
                 str_to_sentence() |>
                 str_replace(" ", "_")) |>
        mutate(id = paste0('{"case": ', case_id,
                           ' ,\"name\":\"', name, '", ',
                           ' ,\"role\":\"', role, '\"}"}')) |>
        select(id,
               case_id,
               name,
               role,
               created_at,
               updated_at)
    }

    if (exists("party")) {
      party <<- bind_rows(party, parties_tmp)
    } else {
      party <<- parties_tmp
    }

    ##### Loop through tables #####
    for (i in 1:length(d)) {

      if ("" %in% names(d[[i]])) {

      } else {
        t <- as.data.frame(d[[i]]) |>
          mutate(district = str_to_upper(district_tmp),
                 case_number = case_number_tmp,
                 case_type = case_type_tmp,
                 year = caseyear_tmp,
                 case_id = case_id_tmp,
                 created_at = Sys.time(),
                 updated_at = Sys.time())

        #### case table ####
        if ("X1" %in% names(t) & str_detect(t[1,2], "Judge:")) {
          caseinfo_tmp <- t |>
            mutate(title = X1 |>
                     str_squish(),
                   date_filed = str_extract(X2, "(?<=Filed: )\\d{1,2}/\\d{1,2}/\\d{1,4}") |>
                     mdy(),
                   date_closed = str_extract(X2, "(?<=Closed: )\\d{1,2}/\\d{1,2}/\\d{1,4}") |>
                     mdy(),
                   judge = str_extract(X2, "(?<=Judge: )(\\w|\\s).*") |>
                     str_squish() |>
                     str_to_upper()) |>
            select(id = case_id,
                   title,
                   district,
                   case_number,
                   case_type,
                   year,
                   date_filed,
                   date_closed,
                   judge,
                   created_at,
                   updated_at) |>
            left_join(open_count_tmp,
                      by = "id")

          if (exists("case")) {case <<- bind_rows(case, caseinfo_tmp)}
          else {case <<- caseinfo_tmp}
        }
        #### attorney table ####
        else if ("Attorney" %in% names(t)) {

          ht2 <- ht
          xml2::xml_find_all(ht2, ".//br") %>%
            xml2::xml_add_sibling("p", " ")

          xml2::xml_find_all(ht2, ".//br") %>%
            xml2::xml_remove()

          d2 <- ht2 |>
            html_nodes("table") |>
            html_table()

          atts_tmp <- d2[[i]] |>
            mutate(district = str_to_upper(district_tmp),
                   case_number = case_number_tmp,
                   case_type = case_type_tmp,
                   year = caseyear_tmp,
                   case_id = case_id_tmp,
                   created_at = Sys.time(),
                   updated_at = Sys.time()) |>
            mutate(name = str_extract(Attorney, "^.*?(?=\\s{5,10}|\\(|$)") |>
                     str_squish(),
                   bar_number = str_extract(Attorney, "(?<=Bar #)\\d*") |>
                     str_squish(),
                   address = str_extract(Attorney, "(?<=\\s{5,10}|\\)).*") |>
                     str_squish(),
                   parties = `Represented Parties`|>
                     str_squish() |>
                     str_remove(",$"),
                   id = if_else(is.na(bar_number),
                                paste0('{"case": ', case_id, ' ,\"name\":\"', name, '\"}"}'),
                                bar_number
                   )) |>
            mutate(parties = if_else(all(is.na(parties)),
                                     NA_character_,
                                     paste0("{", parties, "}",
                                            collapse = ","))) |>
            select(id,
                   name,
                   bar_number,
                   address,
                   parties,
                   created_at,
                   updated_at, case_id)

          if (exists("attorney")) {attorney <<- bind_rows(attorney, atts_tmp)}
          else {attorney <<- atts_tmp}
        }
        #### event table ####
        else if ("Event" %in% names(t)) {

          events_tmp <- t |>
            mutate(date = str_extract(Event, "(?<=day, ).*?(?=$| at )") |>
                     mdy(),
                   rank = row_number(),
                   weekday = str_extract(Event, "^\\w*?(?=,)"),
                   time = if_else(!is.na(str_extract(Event, "\\d{1,2}:\\d{1,2}( |)(AM|PM)")),
                                  str_extract(Event, "\\d{1,2}:\\d{1,2}( |)(AM|PM)"),
                                  "12:00AM"),
                   datetime = paste(date, time) |>
                     ymd_hm(),
                   description = str_extract(Event, "(?<=\\s{4,10}|(AM|PM))(.|\\s)*") |>
                     str_squish(),
                   docket = Docket |>
                     str_squish(),
                   party = Party |>
                     str_squish(),
                   created_at = Sys.time(),
                   updated_at = Sys.time(),
                   id = paste0('{"case": ', case_id, ' ,\"rank\":\"', rank, '\"}"}')) |>
            fill(date) |>
            mutate(
              id = paste0('{"case": ', case_id, ", ",
                          '"rank":', rank, "}"),
              party = paste0('{"case": ', case_id, ", ",
                             '"name": "', party, ', "',
                             '"role": "Defendant"'),
              created_at = Sys.time(),
              updated_at = Sys.time()) |>
            select(id,
                   rank,
                   datetime,
                   description,
                   party,
                   docket,
                   created_at,
                   updated_at,
                   case_id)

          if (exists("event")) {
            event <<- bind_rows(event, events_tmp)
          }
          else {event <<- events_tmp}
        }
        #### count table ####
        else if ("X1" %in% names(t) & str_detect(t[1,1], "Count #")) {
          names(d[[i + 1]])[1] <- "ignore"
          crim_disps_tmp <- d[[i + 1]] |>
            mutate(district = str_to_upper(district_tmp),
                   case_number = case_number_tmp,
                   case_type = str_sub(case_number, 1, 2),
                   case_id = case_id_tmp,
                   year = str_sub(case_number, 4, 7) |>
                     as.numeric(),
                   X1 = as.character(d[[i]][1]),
                   X2 = as.character(d[[i]][2]) |>
                     str_squish()) |>
            select(-ignore) |>
            mutate(number = str_extract(X1, "\\d{1,2}") |>
                     as.numeric(),
                   count_as_filed = str_extract(X2, "(?<=as Filed: ).*?(?=, in violation)"),
                   violation_of = str_extract(X2, "(?<=violation of).*?(?=Date)"),
                   date_of_offense = str_extract(X2, "(?<=Offense: )\\d{1,2}/\\d{1,2}/\\d{1,4}") |>
                     mdy()) |>
            rename(txt = `Disposition Information`) |>
            mutate(disposition = str_extract(txt, "(?<=Disposed:  ).*?(?=,)"),
                   disposition_date = str_extract(txt, "\\d{1,2}/\\d{1,2}/\\d{1,4}") |>
                     mdy(),
                   disposition_detail = str_extract(txt, "(?<=\\d{4}\\.)(.|\\s)*?(?=Count|$)") |>
                     str_squish(),
                   count_as_disposed = str_extract(txt, "(?<=Count as Disposed:)(.|\\s)*?(?=(Viol))") |>
                     str_squish() |>
                     str_to_upper(),
                   disp_stat = str_extract(txt, "(?<=Violation of)(.|\\s)*") |>
                     str_squish(),
                   party = `Party Name`) |>
            group_by(number) |>
            mutate(rank = row_number() - 1,
                   id = paste0('{"case": ', case_id, ", ",
                               '"rank":', rank,
                               ',"number":', number, "}"),
                   party = paste0('{"case": ', case_id, ", ",
                                  '"name": "', party, ', "',
                                  '"role": "Defendant"'),
                   created_at = Sys.time(),
                   updated_at = Sys.time()) |>
            select(id,
                   number, rank,
                   party,
                   count_as_filed, count_as_disposed,
                   date_of_offense,
                   party,
                   disposition,
                   violation_of,
                   disposition,
                   disposition_detail,
                   disposition_date,
                   count_as_disposed)

          if (exists("count_tbl")) {count_tbl <<- bind_rows(count_tbl, crim_disps_tmp)}
          else {count_tbl <<- crim_disps_tmp}
        }

        ##### issue table ####
        else if ("X1" %in% names(t) & str_detect(t[1,1], "Issue #")) {
          civ_disps_tmp <- d[[i + 1]][2:3] |>
            mutate(case_id = case_id_tmp,
                   X1 = as.character(d[[i]][1]),
                   X2 = as.character(d[[i]][2])) |>
            mutate(number = str_extract(X1, "\\d{1,2}") |>
                     as.numeric(),
                   description = str_extract(X2, "(?<=Issue: )(.|\\s)*?(?=(\\(|Filed))") |>
                     str_trim() |>
                     str_to_upper(),
                   filed_by = str_extract(X2, "(?<=Filed By:)(.|\\s)*?(?=File)") |>
                     str_trim(),
                   filed_date = str_extract(X2, "(?<=Filed Date:)(.|\\s)*?$") |>
                     str_trim() |>
                     mdy()) |>
            rename(txt = `Disposition Information`) |>
            mutate(disposition = str_extract(txt, "(?<=Disposed:  ).*?(?=,)"),
                   disposition_detail = str_extract(txt, "(?<=\\d{4}\\.)(.|\\s)*") |>
                     str_trim(),
                   disposition_date = str_extract(txt, "\\d{1,2}/\\d{1,2}/\\d{1,4}") |>
                     mdy(),
                   party = `Party Name` |>
                     str_remove("Defendant:") |>
                     str_squish()) |>
            mutate(rank = row_number(),
                   id = paste0('{"case": ', case_id, ", ",
                               '"rank":', rank,
                               ',"number":', number, "}"),
                   party = paste0('{"case": ', case_id, ", ",
                                  '"name": "', party, '", ',
                                  '"role": "Defendant"'),
                   created_at = Sys.time(),
                   updated_at = Sys.time()) |>
            select(id, rank, filed_by,
                   filed_date, party,
                   disposition,
                   disposition_date, description,
                   created_at, updated_at, number,
                   case_id)

          if (exists("issue")) {issue <<- bind_rows(issue, civ_disps_tmp)}
          else {issue <<- civ_disps_tmp}
        }
        else if ("Code" %in% names(t) & "Amount" %in% names(t)) {
          # minute table ####
          docs <- ht |>
            html_nodes("p a") |>
            html_attrs() |>
            unlist()

          docs <- docs[str_detect(docs, "GetDocument")] |>
            as_tibble() |>
            mutate(document = str_extract(value, "(?<=bc=)\\d{1,20}")) |>
            group_by(document) |>
            summarize(links = paste0("{",
                                     paste0("https://www.oscn.net/dockets/",
                                            value,
                                            collapse = ","),
                                     "}"))

          mins_tmp <- t |>
            mutate(amount = str_remove_all(Amount, "(\\s|\\$|,)") |>
                     as.numeric(),
                   date = mdy(Date),
                   rank = row_number(),
                   party = Party,
                   case_id = case_id_tmp) |>
            fill(date) |>
            group_by(date) |>
            mutate(id = paste0('{"case": ', case_id, ", ",
                               '"rank":', rank, "}"),
                   party = if_else(!is.na(party),
                                   paste0('{"case": ', case_id, ", ",
                                          '"name": "', party, '", ',
                                          '"role": "Defendant"'),
                                   NA_character_),
                   count = if_else(!is.na(Count),
                                   paste0('{"case": ', case_id, ", ",
                                          '"rank":', Count, "}"),
                                   NA_character_),
                   created_at = Sys.time(),
                   updated_at = Sys.time(),
                   document = str_extract(Description, "(?<=Document Available \\(#)\\d{1,30}")) |>
            left_join(docs,
                      by = "document") |>
            select(id, case_id,
                   party,
                   date,
                   rank,
                   count,
                   code = Code,
                   description = Description,
                   amount,
                   links,
                   created_at,
                   updated_at)

          if (exists("minute_tbl")) {minute_tbl <<- bind_rows(minute_tbl, mins_tmp)}
          else {minute_tbl <<- mins_tmp}

        }
      }
    }

    if (!str_detect(ht, "Citation Information")) { }
    else {
      c <- ht |>
        html_nodes("blockquote") |>
        html_text() |>
        as.tibble() |>
        separate(value, into = paste0("col", 1:30), sep = "\\s{3,}") |>
        gather(cno, c, contains("col")) |>
        separate(c, into = c("varname", "value"), sep = ":") %>%
        select(-cno) %>%
        filter(!is.na(varname), varname != "") %>%
        spread(varname, value) %>%
        mutate(district = str_to_upper(district_tmp),
               case_number = case_number_tmp,
               id = case_id_tmp)

      if ("Accident" %in% names(c)) {
        c <- c |>
          janitor::clean_names() |>
          mutate(bond_amount = str_remove_all(bond_amount, "\\$|\\s") |>
                   as.numeric(),
                 information_date = mdy(information_date),
                 north_location = if_else(north_location == "",
                                          NA_character_,
                                          north_location),
                 across(is.character, str_squish),
                 created_at = Sys.time(),
                 updated_at = Sys.time(),
                 case_id = case_id_tmp) |>
          select(id,
                 arresting_agency,
                 location = location_of_offense,
                 north_location,
                 east_control,
                 county,
                 citation_number,
                 license_class,
                 license_endorsements,
                 employer,
                 violation_type,
                 vehicle_make,
                 vehicle_model,
                 vehicle_body_style,
                 vehicle_color,
                 vehicle_tag,
                 vehicle_tag_year,
                 vehicle_tag_issuer,
                 bond_amount,
                 created_at,
                 updated_at,
                 case_id
          )
      }

      if (exists("citation")) {citation <<- bind_rows(citation, c)}
      else {citations <<- c}
    }

    #### If an ODCR county, navigate to ODCR case page to scrape payments table
    oscn_counties <- court_ref[court_ref$site == "OSCN", ]$court |>
      str_replace("ROGERMILLS", "ROGER MILLS")

    if (!district_tmp %in% oscn_counties) {

      u <- court_ref[court_ref$court == district_tmp, "url_pattern"] %>%
        str_replace("XX", paste0(case_type_tmp, "+")) %>%
        str_replace("YY", str_sub(caseyear_tmp, 3, 4)) %>%
        str_replace("ZZZZ", str_pad(caseseq_tmp |>
                                      as.integer(),
                                    side = "left",
                                    pad = 0,
                                    width = if_else(str_detect(caseseq_tmp, "[:alpha:]"),
                                                    5,
                                                    4)))

      ht_odcr <- try(read_html(httr::GET(u,
                                         user_agent("1ecbd577-793f-4a38-b82f-e361ed335168"))))

      d_odcr <- ht_odcr %>%
        html_nodes("table") %>%
        html_table()

      t_odcr <- d_odcr |>
        pluck(length(d_odcr))

      if ("Amount" %in% names(t_odcr)) {
        odcr_pays_tmp <- t_odcr  |>
          mutate(case_id = case_id_tmp) |>
          filter(Date != "Grand Total")  |>
          mutate(date = mdy(Date),
                 amount = Amount  |>
                   str_remove_all("\\$|,") |>
                   as.numeric()) |>
          mutate(id = paste0('{"case": ', case_id,
                             ' ,\"row\":\"', row_number(), '\"}"}'))  |>
          select(id, date, description = Description, amount, case_id)

        if (exists("odcr_pays")) {odcr_pays <<- bind_rows(odcr_pays, odcr_pays_tmp)
        } else {odcr_pays <<- odcr_pays_tmp}
      }
    }

    ##### Navigate to party urls and extract information ####
    ### Get list of party links
    l <- ht |>
      html_nodes("p a") |>
      html_attrs() |>
      unlist()

    l <- l[str_detect(l, "GetParty")] |>
      as_tibble()

    if (nrow(l) == 0) {} else {
      ### Loop through party links

      for (k in 1:nrow(l)) {
        p <- try(read_html(
          httr::GET(paste0("http://www.oscn.net/dockets/", l[k, 1]),
                    user_agent("1ecbd577-793f-4a38-b82f-e361ed335168")
          )
        ))

        if (length(p) == 1) {
          if (class(p) == "try-error") {
          }
        } else {

          party_id_tmp <- str_extract(l[k, 1], "(?<=id=).*")

          p <- p |>
            html_nodes("table") |>
            html_table()

          for (i in 1:length(p)) {
            if ("Requested Party" %in% names(p[[i]])) {
              alias_tmp <- p[[i]] |>
                mutate(name = `Requested Party`,
                       alias = `Alias or Alternate Names`) |>
                mutate(id = party_id_tmp,
                       case_id = case_id_tmp) |>
                group_by(name, id) |>
                summarize(aliases = paste0("{",
                                           paste0(alias,
                                                  collapse = ","),
                                           "}"),
                          .groups = "keep") |>
                mutate(aliases = str_remove(aliases, "None Found\\."))

            } else if ("Marital Status" %in% names(p[[i]])) {
              pprofile <- p[[i]] |>
                rename(bd = `Birth Month and Year`) |>
                mutate(id = party_id_tmp,
                       birth_month = str_extract(bd, "\\d{1,2}") |>
                         as.numeric(),
                       birth_year = str_extract(bd, "\\d{1,2}$") |>
                         as.numeric()) |>
                mutate(birth_year = if_else(birth_year > year(Sys.Date()) - 15,
                                            birth_year + 2000,
                                            birth_year + 1900))

            } else if ("Address" %in% names(p[[i]])) {
              paddr_tmp <<- p[[i]] |>
                mutate(record_date = mdy(`Record Date`),
                       address = as.character(Address) |>
                         str_squish(),
                       postal_code = str_extract(Address, "\\d{5}$"),
                       oscn_id = party_id_tmp) |>
                select(id = oscn_id,
                       record_date,
                       address) |>
                group_by(id) |>
                summarize(addresses = paste0("{",
                                             paste0('"', address, '"',
                                                    collapse = ","),
                                             "}"))

              person_record_tmp <- alias_tmp |>
                left_join(pprofile,
                          by = "id") |>
                left_join(paddr_tmp,
                          by = "id") |>
                mutate(oscn_id = as.numeric(id),
                       created_at = Sys.time(),
                       updated_at = Sys.time()) |>
                select(id,
                       name,
                       oscn_id,
                       birth_month,
                       birth_year,
                       aliases,
                       addresses,
                       created_at,
                       updated_at)

              if (exists("person_record")) {person_record <<- bind_rows(person_record, person_record_tmp)
              } else {
                person_record <<- person_record_tmp }
            }
          }
        }
      }
    }
  }
  message(paste(district_tmp, case_number_tmp, "scraped in", round(Sys.time() - start, 1), "seconds."))

  last_scraped <<- caseseq_tmp

}

ojo_scrape <- function(districts,
                       casetypes,
                       years,
                       case_seqs) {

  for (l in districts) {
    district_tmp <- str_to_upper(l)

    for (m in casetypes) {
      casetype_tmp <- str_to_upper(m)

      for (j in years) {
        caseyear_tmp <- j

        for (k in case_seqs) {

          start <- Sys.time()
          caseseq_tmp <- k
          case_number_tmp <- paste(casetype_tmp, caseyear_tmp,
                                   str_pad(caseseq_tmp, side = "left", width = 5, pad = 0),
                                   sep = "-")

          url <- paste0("http://www.oscn.net/dockets/GetCaseInformation.aspx?db=",
                        district_tmp,
                        "&number=",
                        case_number_tmp)

          ht <- try(read_html(httr::GET(url,
                                        #config = httr::config(ssl_verifypeer = FALSE),
                                        user_agent("1ecbd577-793f-4a38-b82f-e361ed335168"))))

          if (length(ht) == 1) {

            queued_tmp <<- tibble(district = str_to_upper(district_tmp),
                                  case_number = case_number_tmp)

          } else {
            d <- ht %>%
              html_nodes("table") %>%
              html_table()

            #### Skip scraping if case record is empty, record that it's unavailable in oscn_updates ####
            if (length(d) == 0) {
              message(district_tmp, case_number_tmp, " not available")
            } else if (length(d) < 2 & !"Case Number" %in% names(d[[1]])) {

            } else if ("Case Number" %in% names(d[[1]])) {
              ### If page is a list of results, get the case numbers
              pn <- ht %>%
                html_nodes("a") %>%
                html_attrs() |>
                as.character() %>%
                enframe() |>
                filter(str_detect(value, "GetCase")) |>
                select(-name) |>
                distinct()

              ### For case numbers that end in letters, go through each result

              for (r in 1:nrow(pn)) {
                mcd <- try(read_html(
                  httr::GET(paste0("http://www.oscn.net/dockets/", pn[r, 1]),
                            config = httr::config(ssl_verifypeer = FALSE),
                            user_agent("1ecbd577-793f-4a38-b82f-e361ed335168")
                  )
                ))

                if (length(mcd) == 1) {

                } else {

                  parse_page(mcd)

                }
              }
            }
            else {

              parse_page(ht)

            }
          }
        }
      }
    }
  }
}


