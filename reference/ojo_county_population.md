# Get population data by county for Oklahoma from the Census Bureau's American Community Survey

Summarizes and manipulates the PEP series to return Oklahoma's
population by county for desired years. Useful for calculating per
capita rates of county-level data.

## Usage

``` r
ojo_county_population(years)
```

## Arguments

- years:

  A numeric vector of years for which to get population data

## Value

A table of Oklahoma's population by county in each year specified

## Examples

``` r
if (FALSE) { # \dontrun{
ojo_county_population(2018)
} # }
```
