# Get or create the default OJO database connection

This internal function manages a single, default connection object
stored in the package's private environment (`.ojo_env`). It is the
cornerstone of the interactive user experience, ensuring all default
database operations for a given backend share the same connection.

## Usage

``` r
ojo_default_connection(...)
```

## Arguments

- ...:

  Arguments to pass to
  [`ojo_connect()`](https://openjusticeok.github.io/ojodb/reference/ojo_connect.md),
  primarily a `db_config` object to specify the backend (e.g.,
  `db_config = db_config(.driver = "duckdb")`).

## Value

A valid database connection object.

## Details

This function accepts arguments (...) that are passed to
[`ojo_connect()`](https://openjusticeok.github.io/ojodb/reference/ojo_connect.md),
allowing it to manage distinct default connections for different
backends.

The first time this function is called with a unique configuration, it
will:

1.  Call
    [`ojo_connect()`](https://openjusticeok.github.io/ojodb/reference/ojo_connect.md)
    with the provided arguments.

2.  Store the connection in the `.ojo_env` environment under a unique
    key.

3.  Use [`withr::defer()`](https://withr.r-lib.org/reference/defer.html)
    to register a cleanup handler for that connection.
