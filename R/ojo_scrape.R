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

court_ref <- readr::read_csv("inst/extdata/court_reference.csv")

parse_page <- function(ht) {
  rlang::check_installed("httr")
  rlang::check_installed("rvest")
  rlang::check_installed("janitor")
  rlang::check_installed("xml2")

  options(warn = -1)
  start <- Sys.time()

  d <- ht |>
    rvest::html_elements("table") |>
    rvest::html_table()

  case_number_tmp <- stringr::str_extract(d[[1]]$X2, "(?<=No\\. ).*?(?=\r)") |>
    stringr::str_trim()

  district_tmp <- ht |>
    rvest::html_elements("a") |>
    rvest::html_text() |>
    purrr::pluck(18) |>
    stringr::str_remove("County") |>
    stringr::str_trim()

  district_tmp <- dplyr::case_when(
    district_tmp == "BRISTOW" ~ "CREEK (BRISTOW)",
    district_tmp == "HENRYETTA" ~ "OKMULGEE (HENRYETTA)",
    district_tmp == "DRUMRIGHT" ~ "CREEK (DRUMRIGHT)",
    district_tmp == "PONCA CITY" ~ "KAY (PONCA CITY)",
    TRUE ~ district_tmp
  )

  case_type_tmp <- stringr::str_sub(case_number_tmp, 1, 2)

  caseyear_tmp <- case_number_tmp |>
    stringr::str_sub(4, 7) |>
    as.integer()

  caseseq_tmp <- case_number_tmp |>
    stringr::str_remove("[:alpha:]$") |>
    stringr::str_extract("\\d{1,5}$")

  case_id_tmp <- paste0(
    '{"district": "',
    district_tmp,
    '", "case_number": "',
    case_number_tmp,
    '"}'
  )

  #### Skip scraping if case record is empty, record that it's unavailable in oscn_updates ####
  if (length(d) < 2) {
    message(paste("Case information not available for", district_tmp, case_number_tmp))
  } else {
    #### open_count #####
    open_count_tmp <- ht |>
      rvest::html_elements("p") |>
      rvest::html_text() |>
      tibble::enframe() |>
      dplyr::mutate(open_count = stringr::str_squish(value)) |>
      dplyr::filter(stringr::str_detect(open_count, "^\\d{1,2}\\.")) |>
      dplyr::mutate(id = case_id_tmp) |>
      dplyr::select(
        id,
        open_count
      ) |>
      dplyr::group_by(id) |>
      dplyr::summarize(
        open_counts = paste0(
          "{",
          paste0('"', open_count, '"',
            collapse = ","
          ),
          "}"
        ),
        .groups = "keep"
      )

    ### Party names list ####

    pn <- ht |>
      rvest::html_elements("p")

    pn <- pn[2] |>
      as.character() |>
      tibble::enframe() |>
      dplyr::mutate(value = as.character(value) |>
        stringr::str_remove_all("<(/|)p>") |>
        stringr::str_squish())

    if (stringr::str_detect(pn[1, "value"], "href")) {
      parties_tmp <- pn |>
        tidyr::separate(value,
          into = paste0("party", 1:20), sep = "<br>",
          extra = "drop", fill = "right"
        ) |>
        tidyr::pivot_longer(cols = dplyr::contains("party"), names_to = "pnum", values_to = "text") |>
        dplyr::filter(text != "", !is.na(text)) |>
        dplyr::mutate(
          case_id = case_id_tmp,
          oscn_id = stringr::str_extract(text, "(?<=id=)\\d{1,20}"),
          created_at = Sys.time(),
          updated_at = Sys.time(),
          name = stringr::str_extract(text, "(?<=\\>).*(?=\\<)") |>
            stringr::str_trim(),
          role = text |>
            stringr::str_extract("(?<=\\>, ).*?$") |>
            stringr::str_trim() |>
            stringr::str_to_sentence() |>
            stringr::str_replace(" ", "_")
        ) |>
        dplyr::mutate(id = paste0(
          '{"case": ', case_id,
          ' ,\"name\":\"', oscn_id, '", ',
          ' ,\"role\":\"', role, '\"}"}'
        )) |>
        dplyr::select(
          id,
          case_id,
          oscn_id,
          name,
          role,
          created_at,
          updated_at
        )
    } else {
      #### party table ####
      parties_tmp <- pn |>
        tidyr::separate(value,
          into = paste0("party", 1:20), sep = "<br>",
          extra = "drop", fill = "right"
        ) |>
        tidyr::pivot_longer(cols = dplyr::contains("party"), names_to = "pnum", values_to = "party_name") |>
        dplyr::filter(party_name != "", !is.na(party_name)) |>
        tidyr::separate(party_name,
          into = c("party_name", "role"),
          sep = ", (?=[[:alpha:]]*$)"
        ) |>
        dplyr::mutate(
          case_id = case_id_tmp,
          created_at = Sys.time(),
          updated_at = Sys.time(),
          name = party_name,
          role = role |>
            stringr::str_trim() |>
            stringr::str_to_sentence() |>
            stringr::str_replace(" ", "_")
        ) |>
        dplyr::mutate(id = paste0(
          '{"case": ', case_id,
          ' ,\"name\":\"', name, '", ',
          ' ,\"role\":\"', role, '\"}"}'
        )) |>
        dplyr::select(
          id,
          case_id,
          name,
          role,
          created_at,
          updated_at
        )
    }

    if (exists("party")) {
      party <<- dplyr::bind_rows(party, parties_tmp)
    } else {
      party <<- parties_tmp
    }

    ##### Loop through tables #####
    for (i in 1:length(d)) {
      if ("" %in% names(d[[i]])) {

      } else {
        t <- as.data.frame(d[[i]]) |>
          dplyr::mutate(
            district = stringr::str_to_upper(district_tmp),
            case_number = case_number_tmp,
            case_type = case_type_tmp,
            year = caseyear_tmp,
            case_id = case_id_tmp,
            created_at = Sys.time(),
            updated_at = Sys.time()
          )

        #### case table ####
        if ("X1" %in% names(t) & stringr::str_detect(t[1, 2], "Judge:")) {
          caseinfo_tmp <- t |>
            dplyr::mutate(
              title = X1 |>
                stringr::str_squish(),
              date_filed = stringr::str_extract(X2, "(?<=Filed: )\\d{1,2}/\\d{1,2}/\\d{1,4}") |>
                lubridate::mdy(),
              date_closed = stringr::str_extract(X2, "(?<=Closed: )\\d{1,2}/\\d{1,2}/\\d{1,4}") |>
                lubridate::mdy(),
              judge = stringr::str_extract(X2, "(?<=Judge: )(\\w|\\s).*") |>
                stringr::str_squish() |>
                stringr::str_to_upper()
            ) |>
            dplyr::select(
              id = case_id,
              title,
              district,
              case_number,
              case_type,
              year,
              date_filed,
              date_closed,
              judge,
              created_at,
              updated_at
            ) |>
            dplyr::left_join(open_count_tmp,
              by = "id"
            )

          if (exists("case")) {
            case <<- dplyr::bind_rows(case, caseinfo_tmp)
          } else {
            case <<- caseinfo_tmp
          }
        }
        #### attorney table ####
        else if ("Attorney" %in% names(t)) {
          ht2 <- ht
          xml2::xml_find_all(ht2, ".//br") |>
            xml2::xml_add_sibling("p", " ")

          xml2::xml_find_all(ht2, ".//br") |>
            xml2::xml_remove()

          d2 <- ht2 |>
            rvest::html_elements("table") |>
            rvest::html_table()

          atts_tmp <- d2[[i]] |>
            dplyr::mutate(
              district = stringr::str_to_upper(district_tmp),
              case_number = case_number_tmp,
              case_type = case_type_tmp,
              year = caseyear_tmp,
              case_id = case_id_tmp,
              created_at = Sys.time(),
              updated_at = Sys.time()
            ) |>
            dplyr::mutate(
              name = stringr::str_extract(Attorney, "^.*?(?=\\s{5,10}|\\(|$)") |>
                stringr::str_squish(),
              bar_number = stringr::str_extract(Attorney, "(?<=Bar #)\\d*") |>
                stringr::str_squish(),
              address = stringr::str_extract(Attorney, "(?<=\\s{5,10}|\\)).*") |>
                stringr::str_squish(),
              parties = `Represented Parties` |>
                stringr::str_squish() |>
                stringr::str_remove(",$"),
              id = dplyr::if_else(is.na(bar_number),
                paste0('{"case": ', case_id, ' ,\"name\":\"', name, '\"}"}'),
                bar_number
              )
            ) |>
            dplyr::mutate(parties = dplyr::if_else(all(is.na(parties)),
              NA_character_,
              paste0("{", parties, "}",
                collapse = ","
              )
            )) |>
            dplyr::select(
              id,
              name,
              bar_number,
              address,
              parties,
              created_at,
              updated_at, case_id
            )

          if (exists("attorney")) {
            attorney <<- dplyr::bind_rows(attorney, atts_tmp)
          } else {
            attorney <<- atts_tmp
          }
        }
        #### event table ####
        else if ("Event" %in% names(t)) {
          events_tmp <- t |>
            dplyr::mutate(
              date = stringr::str_extract(Event, "(?<=day, ).*?(?=$| at )") |>
                lubridate::mdy(),
              rank = dplyr::row_number(),
              weekday = stringr::str_extract(Event, "^\\w*?(?=,)"),
              time = dplyr::if_else(!is.na(stringr::str_extract(Event, "\\d{1,2}:\\d{1,2}( |)(AM|PM)")),
                stringr::str_extract(Event, "\\d{1,2}:\\d{1,2}( |)(AM|PM)"),
                "12:00AM"
              ),
              datetime = paste(date, time) |>
                lubridate::ymd_hm(),
              description = stringr::str_extract(Event, "(?<=\\s{4,10}|(AM|PM))(.|\\s)*") |>
                stringr::str_squish(),
              docket = Docket |>
                stringr::str_squish(),
              party = Party |>
                stringr::str_squish(),
              created_at = Sys.time(),
              updated_at = Sys.time(),
              id = paste0('{"case": ', case_id, ' ,\"rank\":\"', rank, '\"}"}')
            ) |>
            tidyr::fill(date) |>
            dplyr::mutate(
              id = paste0(
                '{"case": ', case_id, ", ",
                '"rank":', rank, "}"
              ),
              party = paste0(
                '{"case": ', case_id, ", ",
                '"name": "', party, ', "',
                '"role": "Defendant"'
              ),
              created_at = Sys.time(),
              updated_at = Sys.time()
            ) |>
            dplyr::select(
              id,
              rank,
              datetime,
              description,
              party,
              docket,
              created_at,
              updated_at,
              case_id
            )

          if (exists("event")) {
            event <<- dplyr::bind_rows(event, events_tmp)
          } else {
            event <<- events_tmp
          }
        }
        #### count table ####
        else if ("X1" %in% names(t) & stringr::str_detect(t[1, 1], "Count #")) {
          names(d[[i + 1]])[1] <- "ignore"
          crim_disps_tmp <- d[[i + 1]] |>
            dplyr::mutate(
              district = stringr::str_to_upper(district_tmp),
              case_number = case_number_tmp,
              case_type = stringr::str_sub(case_number, 1, 2),
              case_id = case_id_tmp,
              year = stringr::str_sub(case_number, 4, 7) |>
                as.numeric(),
              X1 = as.character(d[[i]][1]),
              X2 = as.character(d[[i]][2]) |>
                stringr::str_squish()
            ) |>
            dplyr::select(-ignore) |>
            dplyr::mutate(
              number = stringr::str_extract(X1, "\\d{1,2}") |>
                as.numeric(),
              count_as_filed = stringr::str_extract(X2, "(?<=as Filed: ).*?(?=, in violation)"),
              violation_of = stringr::str_extract(X2, "(?<=violation of).*?(?=Date)"),
              date_of_offense = stringr::str_extract(X2, "(?<=Offense: )\\d{1,2}/\\d{1,2}/\\d{1,4}") |>
                lubridate::mdy()
            ) |>
            dplyr::rename(txt = `Disposition Information`) |>
            dplyr::mutate(
              disposition = stringr::str_extract(txt, "(?<=Disposed:  ).*?(?=,)"),
              disposition_date = stringr::str_extract(txt, "\\d{1,2}/\\d{1,2}/\\d{1,4}") |>
                lubridate::mdy(),
              disposition_detail = stringr::str_extract(txt, "(?<=\\d{4}\\.)(.|\\s)*?(?=Count|$)") |>
                stringr::str_squish(),
              count_as_disposed = stringr::str_extract(txt, "(?<=Count as Disposed:)(.|\\s)*?(?=(Viol))") |>
                stringr::str_squish() |>
                stringr::str_to_upper(),
              disp_stat = stringr::str_extract(txt, "(?<=Violation of)(.|\\s)*") |>
                stringr::str_squish(),
              party = `Party Name`
            ) |>
            dplyr::group_by(number) |>
            dplyr::mutate(
              rank = dplyr::row_number() - 1,
              id = paste0(
                '{"case": ', case_id, ", ",
                '"rank":', rank,
                ',"number":', number, "}"
              ),
              party = paste0(
                '{"case": ', case_id, ", ",
                '"name": "', party, ', "',
                '"role": "Defendant"'
              ),
              created_at = Sys.time(),
              updated_at = Sys.time()
            ) |>
            dplyr::select(
              id,
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
              count_as_disposed
            )

          if (exists("count_tbl")) {
            count_tbl <<- dplyr::bind_rows(count_tbl, crim_disps_tmp)
          } else {
            count_tbl <<- crim_disps_tmp
          }
        }

        ##### issue table ####
        else if ("X1" %in% names(t) & stringr::str_detect(t[1, 1], "Issue #")) {
          civ_disps_tmp <- d[[i + 1]][2:3] |>
            dplyr::mutate(
              case_id = case_id_tmp,
              X1 = as.character(d[[i]][1]),
              X2 = as.character(d[[i]][2])
            ) |>
            dplyr::mutate(
              number = stringr::str_extract(X1, "\\d{1,2}") |>
                as.numeric(),
              description = stringr::str_extract(X2, "(?<=Issue: )(.|\\s)*?(?=(\\(|Filed))") |>
                stringr::str_trim() |>
                stringr::str_to_upper(),
              filed_by = stringr::str_extract(X2, "(?<=Filed By:)(.|\\s)*?(?=File)") |>
                stringr::str_trim(),
              filed_date = stringr::str_extract(X2, "(?<=Filed Date:)(.|\\s)*?$") |>
                stringr::str_trim() |>
                lubridate::mdy()
            ) |>
            dplyr::rename(txt = `Disposition Information`) |>
            dplyr::mutate(
              disposition = stringr::str_extract(txt, "(?<=Disposed:  ).*?(?=,)"),
              disposition_detail = stringr::str_extract(txt, "(?<=\\d{4}\\.)(.|\\s)*") |>
                stringr::str_trim(),
              disposition_date = stringr::str_extract(txt, "\\d{1,2}/\\d{1,2}/\\d{1,4}") |>
                lubridate::mdy(),
              party = `Party Name` |>
                stringr::str_remove("Defendant:") |>
                stringr::str_squish()
            ) |>
            dplyr::mutate(
              rank = dplyr::row_number(),
              id = paste0(
                '{"case": ', case_id, ", ",
                '"rank":', rank,
                ',"number":', number, "}"
              ),
              party = paste0(
                '{"case": ', case_id, ", ",
                '"name": "', party, '", ',
                '"role": "Defendant"'
              ),
              created_at = Sys.time(),
              updated_at = Sys.time()
            ) |>
            dplyr::select(
              id, rank, filed_by,
              filed_date, party,
              disposition,
              disposition_date, description,
              created_at, updated_at, number,
              case_id
            )

          if (exists("issue")) {
            issue <<- dplyr::bind_rows(issue, civ_disps_tmp)
          } else {
            issue <<- civ_disps_tmp
          }
        } else if ("Code" %in% names(t) & "Amount" %in% names(t)) {
          # minute table ####
          docs <- ht |>
            rvest::html_elements("p a") |>
            rvest::html_attrs() |>
            unlist()

          docs <- docs[stringr::str_detect(docs, "GetDocument")] |>
            dplyr::as_tibble() |>
            dplyr::mutate(document = stringr::str_extract(value, "(?<=bc=)\\d{1,20}")) |>
            dplyr::group_by(document) |>
            dplyr::summarize(links = paste0(
              "{",
              paste0("https://www.oscn.net/dockets/",
                value,
                collapse = ","
              ),
              "}"
            ))

          mins_tmp <- t |>
            dplyr::mutate(
              amount = stringr::str_remove_all(Amount, "(\\s|\\$|,)") |>
                as.numeric(),
              date = lubridate::mdy(Date),
              rank = dplyr::row_number(),
              party = Party,
              case_id = case_id_tmp
            ) |>
            tidyr::fill(date) |>
            dplyr::group_by(date) |>
            dplyr::mutate(
              id = paste0(
                '{"case": ', case_id, ", ",
                '"rank":', rank, "}"
              ),
              party = dplyr::if_else(!is.na(party),
                paste0(
                  '{"case": ', case_id, ", ",
                  '"name": "', party, '", ',
                  '"role": "Defendant"'
                ),
                NA_character_
              ),
              count = dplyr::if_else(!is.na(Count),
                paste0(
                  '{"case": ', case_id, ", ",
                  '"rank":', Count, "}"
                ),
                NA_character_
              ),
              created_at = Sys.time(),
              updated_at = Sys.time(),
              document = stringr::str_extract(Description, "(?<=Document Available \\(#)\\d{1,30}")
            ) |>
            dplyr::left_join(docs,
              by = "document"
            ) |>
            dplyr::select(id, case_id,
              party,
              date,
              rank,
              count,
              code = Code,
              description = Description,
              amount,
              links,
              created_at,
              updated_at
            )

          if (exists("minute_tbl")) {
            minute_tbl <<- dplyr::bind_rows(minute_tbl, mins_tmp)
          } else {
            minute_tbl <<- mins_tmp
          }
        }
      }
    }

    if (!any(stringr::str_detect(t, "Citation Information"))) { } else {
      citation_tmp <- ht |>
        rvest::html_elements("blockquote") |>
        rvest::html_text() |>
        dplyr::as_tibble() |>
        tidyr::separate(value, into = paste0("col", 1:30), sep = "\\s{3,}") |>
        tidyr::gather(cno, c, dplyr::contains("col")) |>
        tidyr::separate(c, into = c("varname", "value"), sep = ":") |>
        dplyr::select(-cno) |>
        dplyr::filter(!is.na(varname), varname != "") |>
        tidyr::spread(varname, value) |>
        dplyr::mutate(
          district = stringr::str_to_upper(district_tmp),
          case_number = case_number_tmp,
          id = case_id_tmp
        )

      if ("Accident" %in% names(citation_tmp)) {
        citation_tmp <- citation_tmp |>
          janitor::clean_names() |>
          dplyr::mutate(
            bond_amount = stringr::str_remove_all(bond_amount, "\\$|\\s") |>
              as.numeric(),
            information_date = lubridate::mdy(information_date),
            north_location = dplyr::if_else(north_location == "",
              NA_character_,
              north_location
            ),
            dplyr::across(is.character, stringr::str_squish),
            created_at = Sys.time(),
            updated_at = Sys.time(),
            case_id = case_id_tmp
          ) |>
          dplyr::select(id,
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

      if (exists("citation_tbl")) {
        citation_tbl <<- dplyr::bind_rows(citation_tbl, citation_tmp)
      } else {
        citation_tbl <<- citation_tmp
      }
    }

    #### If an ODCR county, navigate to ODCR case page to scrape payments table
    oscn_counties <- court_ref[court_ref$site == "OSCN", ]$court |>
      stringr::str_replace("ROGERMILLS", "ROGER MILLS")

    if (!district_tmp %in% oscn_counties) {
      u <- court_ref[court_ref$court == district_tmp, "url_pattern"] |>
        stringr::str_replace("XX", paste0(case_type_tmp, "+")) |>
        stringr::str_replace("YY", stringr::str_sub(caseyear_tmp, 3, 4)) |>
        stringr::str_replace("ZZZZ", stringr::str_pad(
          caseseq_tmp |>
            as.integer(),
          side = "left",
          pad = 0,
          width = dplyr::if_else(stringr::str_detect(caseseq_tmp, "[:alpha:]"),
            5,
            4
          )
        ))

      ht_odcr <- try(rvest::read_html(httr::GET(
        u,
        httr::user_agent("1ecbd577-793f-4a38-b82f-e361ed335168")
      )))

      d_odcr <- ht_odcr |>
        rvest::html_elements("table") |>
        rvest::html_table()

      t_odcr <- d_odcr |>
        purrr::pluck(length(d_odcr))

      if ("Amount" %in% names(t_odcr)) {
        odcr_pays_tmp <- t_odcr |>
          dplyr::mutate(case_id = case_id_tmp) |>
          dplyr::filter(Date != "Grand Total") |>
          dplyr::mutate(
            date = lubridate::mdy(Date),
            amount = Amount |>
              stringr::str_remove_all("\\$|,") |>
              as.numeric()
          ) |>
          dplyr::mutate(id = paste0(
            '{"case": ', case_id,
            ' ,\"row\":\"', dplyr::row_number(), '\"}"}'
          )) |>
          dplyr::select(id, date, description = Description, amount, case_id)

        if (exists("odcr_pays")) {
          odcr_pays <<- dplyr::bind_rows(odcr_pays, odcr_pays_tmp)
        } else {
          odcr_pays <<- odcr_pays_tmp
        }
      }
    }

    ##### Navigate to party urls and extract information ####
    ### Get list of party links
    l <- ht |>
      rvest::html_elements("p a") |>
      rvest::html_attrs() |>
      unlist()

    l <- l[stringr::str_detect(l, "GetParty")] |>
      dplyr::as_tibble()

    if (nrow(l) == 0) {} else {
      ### Loop through party links

      for (k in 1:nrow(l)) {
        p <- try(rvest::read_html(
          httr::GET(
            paste0("http://www.oscn.net/dockets/", l[k, 1]),
            httr::user_agent("1ecbd577-793f-4a38-b82f-e361ed335168")
          )
        ))

        if (length(p) == 1) {
          if (class(p) == "try-error") {
          }
        } else {
          party_id_tmp <- stringr::str_extract(l[k, 1], "(?<=id=).*")

          p <- p |>
            rvest::html_elements("table") |>
            rvest::html_table()

          for (i in 1:length(p)) {
            if ("Requested Party" %in% names(p[[i]])) {
              alias_tmp <- p[[i]] |>
                dplyr::mutate(
                  name = `Requested Party`,
                  alias = `Alias or Alternate Names`
                ) |>
                dplyr::mutate(
                  id = party_id_tmp,
                  case_id = case_id_tmp
                ) |>
                dplyr::group_by(name, id) |>
                dplyr::summarize(
                  aliases = paste0(
                    "{",
                    paste0(alias,
                      collapse = ","
                    ),
                    "}"
                  ),
                  .groups = "keep"
                ) |>
                dplyr::mutate(aliases = stringr::str_remove(aliases, "None Found\\."))
            } else if ("Marital Status" %in% names(p[[i]])) {
              pprofile <- p[[i]] |>
                dplyr::rename(bd = `Birth Month and Year`) |>
                dplyr::mutate(
                  id = party_id_tmp,
                  birth_month = stringr::str_extract(bd, "\\d{1,2}") |>
                    as.numeric(),
                  birth_year = stringr::str_extract(bd, "\\d{1,2}$") |>
                    as.numeric()
                ) |>
                dplyr::mutate(birth_year = dplyr::if_else(birth_year > lubridate::year(Sys.Date()) - 15,
                  birth_year + 2000,
                  birth_year + 1900
                ))
            } else if ("Address" %in% names(p[[i]])) {
              paddr_tmp <<- p[[i]] |>
                dplyr::mutate(
                  record_date = lubridate::mdy(`Record Date`),
                  address = as.character(Address) |>
                    stringr::str_squish(),
                  postal_code = stringr::str_extract(Address, "\\d{5}$"),
                  oscn_id = party_id_tmp
                ) |>
                dplyr::select(
                  id = oscn_id,
                  record_date,
                  address
                ) |>
                dplyr::group_by(id) |>
                dplyr::summarize(addresses = paste0(
                  "{",
                  paste0('"', address, '"',
                    collapse = ","
                  ),
                  "}"
                ))

              person_record_tmp <- alias_tmp |>
                dplyr::left_join(pprofile,
                  by = "id"
                ) |>
                dplyr::left_join(paddr_tmp,
                  by = "id"
                ) |>
                dplyr::mutate(
                  oscn_id = as.numeric(id),
                  created_at = Sys.time(),
                  updated_at = Sys.time()
                ) |>
                dplyr::select(
                  id,
                  name,
                  oscn_id,
                  birth_month,
                  birth_year,
                  aliases,
                  addresses,
                  created_at,
                  updated_at
                )

              if (exists("person_record")) {
                person_record <<- dplyr::bind_rows(person_record, person_record_tmp)
              } else {
                person_record <<- person_record_tmp
              }
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

  rlang::check_installed("httr")
  rlang::check_installed("rvest")
  rlang::check_installed("janitor")
  rlang::check_installed("xml2")

  for (l in districts) {
    district_tmp <- stringr::str_to_upper(l)

    for (m in casetypes) {
      casetype_tmp <- stringr::str_to_upper(m)

      for (j in years) {
        caseyear_tmp <- j

        for (k in case_seqs) {
          start <- Sys.time()
          caseseq_tmp <- k
          case_number_tmp <- paste(casetype_tmp, caseyear_tmp,
            stringr::str_pad(caseseq_tmp, side = "left", width = 5, pad = 0),
            sep = "-"
          )

          url <- paste0(
            "http://www.oscn.net/dockets/GetCaseInformation.aspx?db=",
            district_tmp,
            "&number=",
            case_number_tmp
          )

          ht <- try(rvest::read_html(httr::GET(
            url,
            # config = httr::config(ssl_verifypeer = FALSE),
            httr::user_agent("1ecbd577-793f-4a38-b82f-e361ed335168")
          )))

          if (length(ht) == 1) {
            queued_tmp <<- dplyr::tibble(
              district = stringr::str_to_upper(district_tmp),
              case_number = case_number_tmp
            )
          } else {
            d <- ht |>
              rvest::html_elements("table") |>
              rvest::html_table()

            #### Skip scraping if case record is empty, record that it's unavailable in oscn_updates ####
            if (length(d) == 0) {
              message(district_tmp, case_number_tmp, " not available")
            } else if (length(d) < 2 & !"Case Number" %in% names(d[[1]])) {

            } else if ("Case Number" %in% names(d[[1]])) {
              ### If page is a list of results, get the case numbers
              pn <- ht |>
                rvest::html_elements("a") |>
                rvest::html_attrs() |>
                as.character() |>
                tibble::enframe() |>
                dplyr::filter(stringr::str_detect(value, "GetCase")) |>
                dplyr::select(-name) |>
                dplyr::distinct()

              ### For case numbers that end in letters, go through each result

              for (r in 1:nrow(pn)) {
                mcd <- try(rvest::read_html(
                  httr::GET(paste0("http://www.oscn.net/dockets/", pn[r, 1]),
                    config = httr::config(ssl_verifypeer = FALSE),
                    httr::user_agent("1ecbd577-793f-4a38-b82f-e361ed335168")
                  )
                ))

                if (length(mcd) == 1) {

                } else {
                  parse_page(mcd)
                }
              }
            } else {
              parse_page(ht)
            }
          }
        }
      }
    }
  }
}
