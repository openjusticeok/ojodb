# Create database configuration object

Creates a configuration object for OJO database connections with
flexible configuration sources.

## Usage

``` r
db_config(
  ...,
  driver = NULL,
  database = NULL,
  host = NULL,
  port = NULL,
  username = NULL,
  password = NULL,
  ssl_mode = NULL,
  ssl_root_cert = NULL,
  ssl_cert = NULL,
  ssl_key = NULL,
  config_file = NULL
)
```

## Arguments

- ...:

  Placeholder for future args

- driver:

  Database driver name

- database:

  Database name

- host:

  Database host name

- port:

  Database port number

- username:

  Database username

- password:

  Database password

- ssl_mode:

  SSL connection mode

- ssl_root_cert:

  Path to SSL root certificate file

- ssl_cert:

  Path to SSL client certificate file

- ssl_key:

  Path to SSL private key file

- config_file:

  Path to YAML configuration file

## Value

A db_config object (list) containing database connection parameters

## Details

This function creates a database configuration object by reading from
multiple sources with the following precedence (highest to lowest):

1.  Function arguments

2.  YAML configuration file

3.  Environment variables (.Renviron and system)

## Examples

``` r
if (FALSE) { # \dontrun{
# Create config with explicit parameters
config <- db_config(
  driver = "RPostgres",
  database = "ojodb",
  host = "localhost",
  port = "5432",
  username = "user",
  password = "pass",
  ssl_mode = "require"
)

# For local testing where SSL is not configured
local_config <- db_config(
  driver = "RPostgres",
  database = "ojodb",
  host = "localhost",
  port = "5432",
  username = "postgres",
  password = "password",
  ssl_mode = "disable"
)

# Create config from YAML file
config <- db_config(config_file = "~/.ojo_config.yaml")

} # }
```
