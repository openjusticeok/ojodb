# Package index

## Functions for pulling court data

These functions are for pulling and manipulating Oklahoma district court
data scraped from OSCN.net and ODCR.net.

- [`ojo_civ_cases()`](https://openjusticeok.github.io/ojodb/reference/ojo_civ_cases.md)
  : Query civil cases from the OJO database
- [`ojo_crim_cases()`](https://openjusticeok.github.io/ojodb/reference/ojo_crim_cases.md)
  : Query criminal cases from the OJO database
- [`ojo_eviction_cases()`](https://openjusticeok.github.io/ojodb/reference/ojo_eviction_cases.md)
  : OJO Eviction Cases
- [`ojo_add_counts()`](https://openjusticeok.github.io/ojodb/reference/ojo_add_counts.md)
  : Add counts to a tibble of cases
- [`ojo_add_minutes()`](https://openjusticeok.github.io/ojodb/reference/ojo_add_minutes.md)
  : Query minutes for a given case
- [`ojo_add_issues()`](https://openjusticeok.github.io/ojodb/reference/ojo_add_issues.md)
  : Add issues to civil cases
- [`ojo_add_party_details()`](https://openjusticeok.github.io/ojodb/reference/ojo_add_party_details.md)
  : Add party details to a parties query
- [`ojo_collect()`](https://openjusticeok.github.io/ojodb/reference/ojo_collect.md)
  : OJO Collect

## Functions to help aid with analysis

These include some other helpful functions you might use in your
analysis.

- [`ojo_county_population()`](https://openjusticeok.github.io/ojodb/reference/ojo_county_population.md)
  : Get population data by county for Oklahoma from the Census Bureau's
  American Community Survey
- [`ojo_fiscal_year()`](https://openjusticeok.github.io/ojodb/reference/ojo_fiscal_year.md)
  : Determine the Oklahoma state fiscal year that a Date falls in

## Database management functions

These functions allow you to look at and interact with the OJO
database’s structure.

- [`ojo_list_schemas()`](https://openjusticeok.github.io/ojodb/reference/ojo_list_schemas.md)
  : List all schemas on the OJO database
- [`ojo_list_tables()`](https://openjusticeok.github.io/ojodb/reference/ojo_list_tables.md)
  : List all tables on the OJO database
- [`ojo_list_vars()`](https://openjusticeok.github.io/ojodb/reference/ojo_list_vars.md)
  : List all variables in a table on the ojodb database
- [`ojo_case_types()`](https://openjusticeok.github.io/ojodb/reference/ojo_case_types.md)
  : List the case types present in the OJO database

## ‘Behind the scenes’ functions

These are the most basic / fundamental functions in the package.
Generally, we shouldn’t need to use them directly.

- [`ojo_connect()`](https://openjusticeok.github.io/ojodb/reference/ojo_connect.md)
  : Connect to an OJO Database Backend
- [`ojo_pool()`](https://openjusticeok.github.io/ojodb/reference/ojo_pool.md)
  : Create a Database Connection Pool
- [`ojo_connection_string()`](https://openjusticeok.github.io/ojodb/reference/ojo_connection_string.md)
  : Create a Database Connection String
- [`ojo_tbl()`](https://openjusticeok.github.io/ojodb/reference/ojo_tbl.md)
  : Get a table from a database connection
- [`ojo_query()`](https://openjusticeok.github.io/ojodb/reference/ojo_query.md)
  : Send a raw SQL query to the database
- [`ojo_check_ssl()`](https://openjusticeok.github.io/ojodb/reference/ojo_check_ssl.md)
  : Check whether the database connection is using SSL properly
- [`ojo_auth()`](https://openjusticeok.github.io/ojodb/reference/ojo_auth.md)
  : Configure OJO database authentication
- [`db_config()`](https://openjusticeok.github.io/ojodb/reference/db_config.md)
  : Create database configuration object
- [`ojo_version()`](https://openjusticeok.github.io/ojodb/reference/ojo_version.md)
  : OJO Version
