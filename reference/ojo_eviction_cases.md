# OJO Eviction Cases

Collects Oklahoma eviction data for the specified districts (or all of
Oklahoma) for the user specified time frame. If date_end is not
specified, the most up-to-date data will be collected.

Function uses the latest methodology we use for identifying eviction
cases and outcomes in Oklahoma. As that methodology is updated, this
function will be updated to reflect those changes.

## Usage

``` r
ojo_eviction_cases(
  districts = "all",
  date_start = NULL,
  date_end = NULL,
  more_case_variables = NULL,
  more_issue_variables = NULL,
  get_judgments = TRUE,
  ...,
  .con = NULL
)
```

## Arguments

- districts:

  District codes for which to collect data (default is "all", which
  collects data for all districts)

- date_start:

  Start date for the data collection period

- date_end:

  End date for the data collection period (default is NULL, which
  collects the most up-to-date data)

- more_case_variables:

  Additional variables from case table to include in the output

- more_issue_variables:

  Additional variables from issue table to include in the output

- get_judgments:

  Logical value indicating whether to include eviction judgment
  information in the output

- ...:

  Placeholder for additional arguments

- .con:

  The OJO database connection to use

## Value

A dataframe containing eviction data

## Examples

``` r
if (FALSE) { # \dontrun{
ojo_eviction_cases()
ojo_eviction_cases(districts = c("TULSA", "ADAIR"))
ojo_eviction_cases(
  districts = c("TULSA", "ADAIR"),
  date_start = "2020-01-01",
  date_end = "2020-01-31",
  more_issue_variables = disposition_date
)
ojo_eviction_cases(
  districts = c("TULSA", "ADAIR"),
  get_judgments = TRUE
)
} # }
```
