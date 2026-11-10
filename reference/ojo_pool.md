# Create a Database Connection Pool

Creates a managed pool of database connections using the {pool} package.
This is the recommended way to connect to the database for applications
with concurrent users, such as Shiny apps.

## Usage

``` r
ojo_pool(config = NULL, ...)
```

## Arguments

- config:

  A `db_config` object created by
  [`db_config()`](https://openjusticeok.github.io/ojodb/reference/db_config.md).
  If `NULL` (the default), a default configuration is created, which
  will pull from environment variables.

- ...:

  Additional arguments passed on to the underlying
  [`pool::dbPool()`](http://rstudio.github.io/pool/reference/dbPool.md)
  function, such as `minSize`, `maxSize`, or `idleTimeout`.

## Value

A `Pool` object.

## Details

A connection pool is more efficient than managing individual connections
in a concurrent environment. Instead of creating and tearing down a new
connection for each user or session, the pool maintains a set of active
connections that are "checked out" as needed and returned to the pool
when the operation is complete.

This function is a dedicated wrapper around
[`ojo_connect()`](https://openjusticeok.github.io/ojodb/reference/ojo_connect.md)
that forces the creation of a pool.

**Important:** Remember to close the pool with `pool::poolClose(pool)`
when your application shuts down to release all database connections.

## Examples

``` r
if (FALSE) { # \dontrun{
# Create a connection pool using default settings
pool <- ojo_pool()

# Use the pool with ojo_tbl or dplyr
ojo_tbl("case", .con = pool)

# When your application stops, close the pool
pool::poolClose(pool)

# Create a pool for a specific backend (e.g., SQLite)
sqlite_config <- db_config(.driver = "RSQLite", host = "local.db")
sqlite_pool <- ojo_pool(config = sqlite_config)
pool::poolClose(sqlite_pool)
} # }
```
