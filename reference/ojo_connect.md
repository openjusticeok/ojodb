# Connect to an OJO Database Backend

Establishes a single connection to a database, such as the Open Justice
Oklahoma Postgres database or a local DuckDB instance.

## Usage

``` r
ojo_connect(db_config = NULL, ...)
```

## Arguments

- db_config:

  A `db_config` object created by
  [`db_config()`](https://openjusticeok.github.io/ojodb/reference/db_config.md).
  If `NULL` (the default), a default configuration is created by calling
  [`db_config()`](https://openjusticeok.github.io/ojodb/reference/db_config.md)
  with no arguments, which will pull from environment variables.

- ...:

  Placeholder for future arguments.

## Value

A database connection object (e.g., a `PqConnection` or
`duckdb_connection`).

## Details

This function serves as the primary way to create individual database
connection objects. It is designed to be side-effect-free, returning the
connection object directly to the user for manual management. For
creating a managed pool of connections suitable for applications, use
[`ojo_pool()`](https://openjusticeok.github.io/ojodb/reference/ojo_pool.md).

## See also

[`db_config()`](https://openjusticeok.github.io/ojodb/reference/db_config.md)
for creating configuration objects, and
[`ojo_pool()`](https://openjusticeok.github.io/ojodb/reference/ojo_pool.md)
for creating a connection pool.

## Examples

``` r
if (FALSE) { # \dontrun{
# --- Manual Connection Management ---

# 1. Create a configuration
my_config <- db_config(.admin = TRUE)

# 2. Create a connection object
con <- ojo_connect(db_config = my_config)

# 3. Use the connection with dplyr or DBI
dplyr::tbl(con, "case")
DBI::dbListTables(con)

# 4. Close the connection when finished
DBI::dbDisconnect(con)
} # }
```
