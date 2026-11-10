# List all variables in a table on the ojodb database

Query the Open Justice Oklahoma database for the names of all variables
in a table

## Usage

``` r
ojo_list_vars(table, schema = "public", ..., .con = NULL)
```

## Arguments

- table:

  The name of the table to query

- schema:

  The name of the schema to query

- ...:

  Placeholder for additional arguments

- .con:

  The ojodb connection to use

## Value

data, a tibble containing the names of all variables in a table

## Examples

``` r
if (FALSE) { # \dontrun{
ojo_list_vars("case")
ojo_list_vars("inmate", "iic")
} # }
```
