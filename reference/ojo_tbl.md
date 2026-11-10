# Get a table from a database connection

Creates a lazy tibble from a table in a database. This is the primary
way to begin a database query with {ojodb}.

## Usage

``` r
ojo_tbl(table, schema = "public", ..., .con = NULL)
```

## Arguments

- table:

  The name of the table to query.

- schema:

  The name of the schema where the table resides.

- ...:

  Placeholder for additional arguments

- .con:

  A database connection object. If `NULL` (the default), a managed
  default connection will be created and used automatically.

## Value

A lazy tibble with the `ojo_tbl` class.

## Details

For interactive use, if no connection is provided, this function will
automatically create and manage a default connection to the primary OJO
Postgres database. To connect to other backends (like DuckDB or SQLite),
you must first create a connection object with
[`ojo_connect()`](https://openjusticeok.github.io/ojodb/reference/ojo_connect.md)
and pass it via the `.con` argument.

## Examples

``` r
if (FALSE) { # \dontrun{
# Interactively connect to the 'case' table in the default Postgres DB
case_tbl <- ojo_tbl("case")

# Connect to a table on a manually created DuckDB connection
my_duckdb_con <- ojo_connect(db_config = db_config(.driver = "duckdb"))
my_duckdb_tbl <- ojo_tbl("some_table", .con = my_duckdb_con)
} # }
```
