# Configure OJO database authentication

Sets up authentication for the Open Justice Oklahoma database using a
db_config object.

## Usage

``` r
ojo_auth(..., db_config, .install = FALSE, .overwrite = FALSE)
```

## Arguments

- ...:

  Placeholder for future arguments

- db_config:

  A db_config object created by
  [`db_config()`](https://openjusticeok.github.io/ojodb/reference/db_config.md)

- .install:

  Logical indicating whether to save configuration to .Renviron (TRUE)
  or use for current session only (FALSE)

- .overwrite:

  Logical indicating whether to overwrite existing .Renviron entries

## Value

Invisible NULL

## Details

This function takes a db_config object and sets up the database
authentication. When .install = FALSE (default), credentials are only
set for the current R session. When .install = TRUE, credentials are
saved to .Renviron for persistent use.

## Examples

``` r
if (FALSE) { # \dontrun{
# Create config and authenticate for a remote server
config <- db_config(
  driver = "RPostgres",
  database = "ojodb",
  host = "remote.db.com",
  port = "5432",
  username = "user",
  password = "pass",
  ssl_mode = "require"
)
ojo_auth(db_config = config)

# One-liner with explicit parameters
ojo_auth(db_config = db_config(
  driver = "RPostgres",
  database = "ojodb",
  host = "remote.db.com",
  port = "5432",
  username = "user",
  password = "pass",
  ssl_mode = "require"
))

# From config file
ojo_auth(db_config = db_config(config_file = "~/.ojo_config.yaml"))

# Permanent configuration
ojo_auth(
  db_config = db_config(
    driver = "RPostgres",
    database = "ojodb",
    host = "remote.db.com",
    port = "5432",
    username = "user",
    password = "pass",
    ssl_mode = "require"
  ),
  .install = TRUE
)
} # }
```
