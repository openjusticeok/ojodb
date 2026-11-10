# Query criminal cases from the OJO database

Query the Open Justice Oklahoma database for criminal cases with a case
type of 'CM' (misdemeanor) or 'CF' (felony)

## Usage

``` r
ojo_crim_cases(
  districts = "all",
  vars = NULL,
  case_types = c("CM", "CF", "TR"),
  file_years = 2000:lubridate::year(Sys.Date()),
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

data, a lazy tibble containing the resulting criminal cases

## Examples

``` r
if (FALSE) { # \dontrun{
ojo_crim_cases()
ojo_crim_cases(districts = c("TULSA", "ADAIR"))
ojo_crim_cases(vars = "all")
ojo_crim_cases(vars = c("updated_at", "created_at"))
} # }
```
