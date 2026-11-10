# Send a raw SQL query to the database

Executes a raw SQL query against a database connection. This function is
backend-agnostic and will send the query to any valid DBI connection.

## Usage

``` r
ojo_query(query, ..., .con = NULL)
```

## Arguments

- query:

  The SQL query to send to the database. The user is responsible for
  ensuring the syntax is correct for the target database backend.

- ...:

  Placeholder for additional arguments

- .con:

  The database connection to use. If `NULL`, a default connection to the
  primary Postgres database will be created and used.

## Value

A lazy tibble with the `ojo_tbl` class.

## Examples

``` r
if (FALSE) { # \dontrun{
# Run a query against the default Postgres database
ojo_query("SELECT * FROM case LIMIT 10")

# Run a query against a manual DuckDB connection
duck_con <- ojo_connect(db_config = db_config(.driver = "duckdb"))
ojo_query("SELECT * FROM 'my_duck_db_file.parquet' LIMIT 5", .con = duck_con)
} # }
```
