# Create a Database Connection String

Generates a standard connection string URI for various database backends
from an `ojo_db` configuration object.

## Usage

``` r
ojo_connection_string(config = NULL)
```

## Arguments

- config:

  A `db_config` object created by
  [`db_config()`](https://openjusticeok.github.io/ojodb/reference/db_config.md).
  If `NULL` (the default), a default configuration is created by calling
  [`db_config()`](https://openjusticeok.github.io/ojodb/reference/db_config.md),
  which will pull from environment variables.

## Value

A single character string representing the database connection URI.

## Details

This function is useful for applications or libraries that require a
connection URI. It intelligently constructs the appropriate string based
on the driver specified in the configuration.

- For **PostgreSQL** (`RPostgres`), it creates a `postgresql://` URI.

- For **DuckDB** (`duckdb`), it creates a `duckdb://` URI, treating the
  `host` field in the config as the path to the database file.

## Examples

``` r
if (FALSE) { # \dontrun{
# Create a PostgreSQL connection string from environment variables
ojo_connection_string(config = db_config(driver = "RPostgres"))

# Create a connection string for a local SQLite database file
duckdb_config <- db_config(driver = "duckdb", host = "/path/to/db.duckdb")
ojo_connection_string(config = duckdb_config)
} # }
```
