# Add counts to a tibble of cases

Add counts to a tibble of cases

## Usage

``` r
ojo_add_counts(data, vars = NULL, ..., .con = NULL)
```

## Arguments

- data:

  A tibble returned by an `ojo_` prefixed function

- vars:

  Variable names from the `count` table to include

- ...:

  Placeholder for future arguments

- .con:

  The OJO database connection to use

## Value

A lazy tibble with counts for each case

## Examples

``` r
if (FALSE) { # \dontrun{
ojo_tbl("case") |>
  filter(
    year == 2024,
    district == "CLEVELAND",
    case_type == "CF"
  ) |>
  ojo_add_counts()
} # }
```
