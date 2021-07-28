# ojo_table("case") |>
#   left_join(ojo_table("issue"), by = c("id" = "case_id")) |>
#   select(-starts_with("created"),
#          -starts_with("updated"),
#          -issues) |>
#   rename(id = id.x,
#          issue.id = id.y)
#
# ojo_join <- function() {}
#
# library(dm)
#
# dm <- dbConnect(
#   drv = RPostgres::Postgres(),
#   host = Sys.getenv("NEW_OJO_HOST"),
#   dbname = "ojodb",
#   port = Sys.getenv("NEW_OJO_PORT"),
#   user = Sys.getenv("NEW_OJO_DEFAULT_USER"),
#   password = Sys.getenv("NEW_OJO_DEFAULT_PASS")
# ) |>
#   dm_from_src()
#
# dm <- dm |>
#   dm_add_fk(case, id, party, case_id) |>
#   dm_add_fk(party, id, issue, party)
#
# dm <- dm |>
#   dm_add_fk(case, id, issue, case_id) |>
#   dm_add_fk(issue, party, party, id)
#
# dm |>
#   dm_draw()
#
# dm_flatten_to_tbl(dm, case)
#
# ojo_connect()

ojo_crim_cases <- function() {
  ojo_tbl("case") |>
    filter(case_type %in% c("CF", "CM"))
}

ojo_civ_cases <- function() {

}

ojo_evictions <- function() {

}
