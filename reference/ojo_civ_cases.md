# Query civil cases from the OJO database

Query the Open Justice Oklahoma database for civil cases with a case
type of 'SC' (small claims)

## Usage

``` r
ojo_civ_cases(
  districts = "all",
  vars = NULL,
  case_types = c("CS", "SC", "CJ"),
  file_years = lubridate::year(Sys.Date()),
  ...,
  .con = NULL
)
```

## Arguments

- districts:

  A character vector of districts to query

- vars:

  A character vector of variables to return

- case_types:

  A character vector of case types to query

- file_years:

  A character vector of years to query

- ...:

  Placeholder for additional arguments

- .con:

  The OJO database connection to use

## Value

data, a lazy tibble containing the resulting civil cases

## Examples

``` r
if (FALSE) { # \dontrun{
ojo_civ_cases()
ojo_civ_cases(districts = c("TULSA", "ADAIR"))
ojo_civ_cases(vars = "all")
ojo_civ_cases(vars = c("updated_at", "created_at"))
} # }
```
