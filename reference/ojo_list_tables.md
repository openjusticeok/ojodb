# List all tables on the OJO database

Query the Open Justice Oklahoma database for the names of all tables

## Usage

``` r
ojo_list_tables(schema = "public", ..., .con = NULL)
```

## Arguments

- schema:

  The name of the schema to query

- ...:

  Placeholder for additional arguments

- .con:

  The OJO database connection to use

## Value

data, a tibble containing the names of all tables

## Examples

``` r
if (FALSE) { # \dontrun{
ojo_list_tables()
ojo_list_tables("all")
ojo_list_tables("iic")
} # }
```
