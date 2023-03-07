# ojo_connect

## Overview

### Scenario 1

#### UX
1. An ojo analyst is working in an interactive session
2. They want to pull in a dataset from the database
3. They don't want to have to manually connect to the database with ojo_connect
4. They don't want to have to think about closing the connection either

#### Solution
* ojo_connect is called automatically when a dataset is pulled in from the database
* The connection parameters are automatically pulled from environment variables


### Scenario 2

#### UX
1. An ojo analyst is rendering a report
2. They want to pull in a dataset from the database
3. They don't want to have to manually connect to the database with ojo_connect
4. They don't want to have to think about closing the connection either

#### Solution
* ojo_connect is called automatically when a dataset is pulled in from the database
* The connection parameters are automatically pulled from environment variables
* The connection object is closed when the report is done rendering
* The connection object is only created once per report, unless the report is re-rendered, or ojo_connect is called manually

### Scenario 3

#### UX
1. An ojo engineer is writing a function to pull in a dataset from the database
2. They want their function to be able to connect to the database automatically or by passing in a connection object as an argument

#### Solution


### Scenario 4

#### UX
1. An ojo engineer is making a Github Action to run the package tests
2. They want to be able to connect to the database in the Github Action


## Constraints
* The connection object is created with `pool::dbPool` and the `RPostgres::Postgres()` driver
* `ojo_tbl` returns a lazy tbl object
* The connection object is closed when the session ends, not when the dataset is pulled in
* Consecutive calls to `ojo_tbl` will use the same connection object
* The default schema is `public`, but can be changed with `ojo_connect(schema = "my_schema")`
* There is no default table -- the user must specify a table name

## Implementation

```r
# ojo_connect.R


#' @title OJO Connect
#'
#' @description Connect to the Open Justice Oklahoma database
#'
#' @details
#' Opens a connection to the Open Justice Oklahoma database using credentials stored in the .Renviron file.
#' If no credentials exist, prompts for user, password, and host name and provides instructions to store them for future sessions.
#'
#' @param host The host name of the database server
#' @param port The port number of the database server
#' @param username The username to use to connect to the database
#' @param password The password to use to connect to the database
#' @param .admin A logical value indicating whether to connect to the database as an administrator
#' @param .overwrite A logical value indicating whether to overwrite the existing .Renviron file
#' @param .install A logical value indicating whether to install the database connection or use it only for the current session
#'
#' @export
#' @returns A database connection object created with `pool::dbPool` and `odbc::odbc`
#'
#' @examples
#' \dontrun{
#' ojo_connect()
#' }
#' @section Side Effects:
#' If either the `.global` argument or `rlang::is_interactive` are `TRUE`, an object named `ojodb` is created in the global environment.
#'
#' @seealso ojo_auth()
#'
ojo_connect <- function(..., .admin = FALSE, .global = rlang::is_interactive()) {

  # If object ojodb is already in the global environment, make sure its a valid pool object and return it
  if (.global && exists("ojodb", envir = .GlobalEnv)) {
    if (inherits(ojodb, "Pool") && pool::dbIsValid(ojodb)) {
      invisible(ojodb)
    } else {
      rlang::abort("The object `ojodb` already exists in the global environment, but it is not a valid database connection.")
    }
  }

  user_type <- if (.admin) "ADMIN" else "DEFAULT"

  if (Sys.getenv("OJO_HOST") == "") {
    rlang::abort("No {tolower(user_type)} configuration for the OJO database was found. Please create one now using `ojo_auth`, or manually, by adding the necessary environment variables with `usethis::edit_r_environ`.")
  }

  conn <- pool::dbPool(
    drv = RPostgres::Postgres(),
    dbname = "ojodb",
    host = Sys.getenv("OJO_HOST"),
    port = Sys.getenv("OJO_PORT"),
    user = Sys.getenv(glue::glue("OJO_{user_type}_USER")),
    password = Sys.getenv(glue::glue("OJO_{user_type}_PASS")),
    sslmode = Sys.getenv("OJO_SSL_MODE"),
    sslrootcert = Sys.getenv("OJO_SSL_ROOT_CERT"),
    sslcert = Sys.getenv("OJO_SSL_CERT"),
    sslkey = Sys.getenv("OJO_SSL_KEY"),
    bigint = "integer",
    ...
  )

  if (.global) {
    assign("ojodb", conn, envir = .GlobalEnv)
    # Defer pool::poolClose() until the end of the session
    withr::defer(
      pool::poolClose(ojodb),
      envir = .GlobalEnv
    )
  }

  invisible(conn)
}


```

```r
# ojo_tbl.R


#' Identify a table from the OJO database
#'
#' Identifies a table in the OJO database from which to query data. Remember to run \code{connect_ojo()} to establish a connection before attempting to query and to close the connection afterwards with \code{disconnect_ojo()}.
#'
#' @aliases ojo_tbl
#'
#' @param table The name of a table in the OJO database. To get a list of tables, run \code{ojo_list_tables()}
#' @param schema The name of a schema in the OJO database. To get a list of schemas, run \code{ojo_list_schemas()}
#' 
#' @export ojo_tbl
#' @return A pointer to a table that can be passed to dplyr functions and/or pulled into a dataframe using \code{ojo_collect()}
#' @examples
#' \dontrun{
#' # Identifies the table
#' ojo_tbl("case")
#'
#' # Pulls down case information data for every Tulsa felony filed in 2020 into a dataframe d
#' d <- ojo_tbl("case") %>%
#'   filter(district == "TULSA", case_type == "CF", year == 2020) %>%
#'   collect()
#' }
#' @seealso ojo_list_tables(), ojo_list_vars(), ojo_list_schemas()
#'
ojo_tbl <- function(table, schema = "public", ..., .con = ojo_connect()) {
  .con |>
    dplyr::tbl(dbplyr::in_schema(schema, table))
}

```

```yaml
# .github/workflows/test.yml

# Workflow derived from https://github.com/r-lib/actions/tree/v2/examples
# Need help debugging build failures? Start at https://github.com/r-lib/actions#where-to-find-help
on:
  [push]

name: test-coverage

jobs:
  test-coverage:
    runs-on: ubuntu-latest
    env:
      GITHUB_PAT: ${{ secrets.GITHUB_TOKEN }}

    steps:
      - uses: actions/checkout@v3

      - uses: r-lib/actions/setup-r@v2
        with:
          use-public-rspm: true

      - uses: r-lib/actions/setup-r-dependencies@v2
        with:
          extra-packages: any::covr
          needs: coverage

      - name: Test coverage
        run: |
          covr::codecov(
            quiet = FALSE,
            clean = FALSE,
            install_path = file.path(Sys.getenv("RUNNER_TEMP"), "package")
          )
        shell: Rscript {0}

      - name: Show testthat output
        if: always()
        run: |
          ## --------------------------------------------------------------------
          find ${{ runner.temp }}/package -name 'testthat.Rout*' -exec cat '{}' \; || true
        shell: bash

      - name: Upload test results
        if: failure()
        uses: actions/upload-artifact@v3
        with:
          name: coverage-test-failures
          path: ${{ runner.temp }}/package

```
