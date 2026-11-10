# Add party details to a parties query

Query the Open Justice Oklahoma database for party details

## Usage

``` r
ojo_add_party_details(data, vars = NULL, ..., .con = NULL)
```

## Arguments

- data:

  A lazy tibble containing the results of a parties query

- vars:

  A character vector of variables to return

- ...:

  Placeholder for additional arguments

- .con:

  The OJO database connection to use

## Value

data, a lazy tibble containing the resulting party details

## Examples

``` r
if (FALSE) { # \dontrun{
ojo_add_party_details()
} # }
```
