# Add issues to civil cases

Add issues to civil cases

## Usage

``` r
ojo_add_issues(data, vars = NULL, ..., .con = NULL)
```

## Arguments

- data:

  A lazy tibble of civil cases

- vars:

  A character vector of variables to return

- ...:

  Placeholder for additional arguments

- .con:

  The OJO database connection to use

## Value

data, a lazy tibble containing the resulting civil cases

## Examples

``` r
if (FALSE) { # \dontrun{
ojo_civ_cases() |>
 ojo_add_issues()
} # }
```
