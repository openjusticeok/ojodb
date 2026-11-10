# Authentication and Configuration

As of version 3.0.0, `ojodb` introduces a unified and flexible system
for managing database configuration and authentication. This vignette
explains how to set up your environment, manage credentials securely,
and migrate your existing workflows from previous versions.

## Configuration Overview

The package uses a configuration object (`db_config`) to store
connection details. This object can be populated from three sources, in
order of precedence:

1.  **Direct Arguments**: Parameters passed directly to
    [`db_config()`](https://openjusticeok.github.io/ojodb/reference/db_config.md).
2.  **Configuration File**: A YAML file (typically
    `~/.ojo_config.yaml`).
3.  **Environment Variables**: System or `.Renviron` variables.

This flexibility allows you to set global defaults using environment
variables while overriding specific parameters for individual scripts or
sessions.

## Setting up Authentication

### Method 1: Environment Variables (Recommended)

The most secure and persistent way to configure `ojodb` is by setting
environment variables in your `.Renviron` file. You can do this easily
using the
[`ojo_auth()`](https://openjusticeok.github.io/ojodb/reference/ojo_auth.md)
function.

1.  **Create a configuration object** with your credentials:

    ``` r

    library(ojodb)

    # Replace with your actual credentials
    my_config <- db_config(
      driver = "RPostgres",
      host = "db.example.com",
      port = "5432",
      database = "ojodb",
      username = "my_user",
      password = "my_password",
      ssl_mode = "require"
    )
    ```

2.  **Save the configuration**:

    Use
    [`ojo_auth()`](https://openjusticeok.github.io/ojodb/reference/ojo_auth.md)
    with `.install = TRUE` to write these settings to your `.Renviron`
    file. This ensures they are available in future R sessions.

    ``` r

    ojo_auth(db_config = my_config, .install = TRUE)
    ```

    *Note: You will need to restart your R session for these changes to
    take effect.*

### Method 2: YAML Configuration File

You can also store credentials in a YAML file. This is useful for
managing multiple configurations or sharing non-sensitive configs.

Create a file named `.ojo_config.yaml` in your home directory (or
elsewhere):

``` yaml
ojodb:
  driver: "RPostgres"
  host: "db.example.com"
  port: "5432"
  database: "ojodb"
  username: "my_user"
  password: "my_password"
  ssl_mode: "require"
```

Then load it:

``` r

config <- db_config(config_file = "~/.ojo_config.yaml")
ojo_connect(db_config = config)
```

### Method 3: Session-Only Configuration

`.install` defaults to `FALSE`, so credentials given to
[`ojo_auth()`](https://openjusticeok.github.io/ojodb/reference/ojo_auth.md)
apply only to the current session and are never written to disk:

``` r

ojo_auth(
  db_config = db_config(
    driver = "RPostgres",
    database = "ojodb",
    host = "db.example.com",
    port = "5432",
    username = "guest",
    password = "guest_pass",
    ssl_mode = "require"
  )
)
```

`driver`, `host`, `port`, `username`, `password`, and `ssl_mode` are all
required by
[`ojo_auth()`](https://openjusticeok.github.io/ojodb/reference/ojo_auth.md).
`database` and the certificate paths are optional, unless `ssl_mode` is
`"verify-ca"` or `"verify-full"`, in which case the certificate files
must exist at the paths you give.

## Connecting to the Database

Once configured (either via `.Renviron` or a config object), connecting
is simple:

``` r

# Uses environment variables by default
con <- ojo_connect()
```

If you need to use a specific configuration object:

``` r

con <- ojo_connect(db_config = my_custom_config)
```

### Connection Pooling

For Shiny applications or long-running scripts, use
[`ojo_pool()`](https://openjusticeok.github.io/ojodb/reference/ojo_pool.md)
to manage connections automatically:

``` r

pool <- ojo_pool()

# Don't forget to close it when done!
pool::poolClose(pool)
```

## Migration Guide (v2 to v3)

### TL;DR

Three things stop a v2 workflow from connecting under v3:

1.  **Your stored credentials are invisible to v3.** v2 wrote
    `OJO_ADMIN_USER`/`OJO_ADMIN_PASS` (or the `DEFAULT` pair); v3 reads
    only `OJO_USER`/`OJO_PASS`.
2.  **[`ojo_auth()`](https://openjusticeok.github.io/ojodb/reference/ojo_auth.md)
    no longer persists by default.** `.install` flipped from `TRUE` to
    `FALSE`, and `.overwrite` from `TRUE` to `FALSE`.
3.  **SSL is no longer configured for you.** v2 hardcoded
    `ssl_mode = "verify-ca"` and expected certificates under
    `~/.postgresql/ojodb/`. v3 uses exactly the `ssl_mode` and
    certificate paths you pass, and finds nothing on its own.

The one call that addresses all three:

``` r

library(ojodb)

ojo_auth(
  db_config = db_config(
    driver = "RPostgres",
    database = "ojodb",
    host = "<host>",
    port = "5432",
    username = "<user>",
    password = "<password>",
    ssl_mode = "verify-ca",
    ssl_root_cert = "~/.postgresql/ojodb/server-ca.pem",
    ssl_cert = "~/.postgresql/ojodb/client-cert.pem",
    ssl_key = "~/.postgresql/ojodb/client-key.pem"
  ),
  .install = TRUE,   # write to ~/.Renviron; without this it is session-only
  .overwrite = TRUE  # remove stale OJO_* lines before writing
)
```

Then restart R (or `readRenviron("~/.Renviron")`) and confirm:

``` r

Sys.getenv(c("OJO_DRIVER", "OJO_USER", "OJO_SSL_MODE"))

con <- ojo_connect()
ojo_check_ssl(.con = con)
```

`.overwrite = TRUE` is what deletes the leftover `OJO_ADMIN_*` and
`OJO_DEFAULT_*` lines. v3 ignores them, so nothing breaks if they stay —
but they will mislead the next person to read your `.Renviron`.

### 1. Environment variables

| v2.11 | v3.0.0 | Notes |
|:---|:---|:---|
| `OJO_ADMIN_USER`, `OJO_DEFAULT_USER` | `OJO_USER` | One identity now; pick admin or not via [`db_config()`](https://openjusticeok.github.io/ojodb/reference/db_config.md) |
| `OJO_ADMIN_PASS`, `OJO_DEFAULT_PASS` | `OJO_PASS` |  |
| *(none — passed as `ojo_connect(.driver =)`)* | `OJO_DRIVER` | Required |
| *(hardcoded `ojodb` in [`ojo_connect()`](https://openjusticeok.github.io/ojodb/reference/ojo_connect.md))* | `OJO_DATABASE` |  |
| `OJO_HOST`, `OJO_PORT` | `OJO_HOST`, `OJO_PORT` | Unchanged |
| `OJO_SSL_MODE` | `OJO_SSL_MODE` | v2 always wrote `verify-ca`; v3 honors whatever you set |
| `OJO_SSL_ROOT_CERT`, `OJO_SSL_CERT`, `OJO_SSL_KEY` | Same names | v2 filled these in for you; v3 needs real paths |

Inside
[`db_config()`](https://openjusticeok.github.io/ojodb/reference/db_config.md)
the precedence is: **arguments \> YAML file \> environment variables**.

### 2. `ojo_auth()` signature

| v2.11 | v3.0.0 |
|:---|:---|
| `ojo_auth(host, port, username, password, ..., .admin = FALSE, .overwrite = TRUE, .install = TRUE)` | `ojo_auth(..., db_config, .install = FALSE, .overwrite = FALSE)` |

- Credentials are no longer positional; they travel in a
  [`db_config()`](https://openjusticeok.github.io/ojodb/reference/db_config.md)
  object.
- `.admin` is gone. Admin access is now just admin credentials passed to
  `db_config(username =, password =)`.
- `.install` and `.overwrite` both defaulted to `TRUE` in v2 and both
  default to `FALSE` now.

### 3. `ojo_connect()` signature

| v2.11 | v3.0.0 |
|:---|:---|
| `ojo_connect(..., .admin, .driver, .global, .env = ojo_env(), .pool)` | `ojo_connect(db_config = NULL, ...)` |

The `.admin`, `.driver`, `.global`, `.env`, and `.pool` arguments were
removed. Their replacements:

| v2 call | v3 replacement |
|:---|:---|
| [`ojo_connect()`](https://openjusticeok.github.io/ojodb/reference/ojo_connect.md) | [`ojo_connect()`](https://openjusticeok.github.io/ojodb/reference/ojo_connect.md) — unchanged |
| `ojo_connect(.driver = "duckdb")` | `ojo_connect(db_config = db_config(driver = "duckdb"))` |
| `ojo_connect(.admin = TRUE)` | `ojo_connect(db_config = db_config(username = <admin user>, password = <admin pass>))` |
| `ojo_connect(.pool = TRUE)` | [`ojo_pool()`](https://openjusticeok.github.io/ojodb/reference/ojo_pool.md) |
| `ojo_connect(.env = my_env)` | removed — [`ojo_default_connection()`](https://openjusticeok.github.io/ojodb/reference/ojo_default_connection.md) owns the cache now |

### 4. No More “Magic” Connections

In previous versions,
[`ojo_connect()`](https://openjusticeok.github.io/ojodb/reference/ojo_connect.md)
stored the connection in a hidden package environment and every function
silently found it. A connection could therefore be opened far from where
it was used, and credentials were re-read from the environment on every
call.

v3 offers two explicit modes instead:

``` r

# 1. Default connection: created on first use, reused, closed at session end.
case_tbl <- ojo_tbl("case")

# 2. Explicit connection: you create it, you close it.
con <- ojo_connect()
case_tbl <- ojo_tbl("case", .con = con)
DBI::dbDisconnect(con)
```

Every query function
([`ojo_tbl()`](https://openjusticeok.github.io/ojodb/reference/ojo_tbl.md),
[`ojo_query()`](https://openjusticeok.github.io/ojodb/reference/ojo_query.md),
[`ojo_list_schemas()`](https://openjusticeok.github.io/ojodb/reference/ojo_list_schemas.md),
[`ojo_list_tables()`](https://openjusticeok.github.io/ojodb/reference/ojo_list_tables.md),
[`ojo_list_vars()`](https://openjusticeok.github.io/ojodb/reference/ojo_list_vars.md),
[`ojo_case_types()`](https://openjusticeok.github.io/ojodb/reference/ojo_case_types.md),
[`ojo_civ_cases()`](https://openjusticeok.github.io/ojodb/reference/ojo_civ_cases.md),
[`ojo_crim_cases()`](https://openjusticeok.github.io/ojodb/reference/ojo_crim_cases.md),
[`ojo_eviction_cases()`](https://openjusticeok.github.io/ojodb/reference/ojo_eviction_cases.md),
[`ojo_add_issues()`](https://openjusticeok.github.io/ojodb/reference/ojo_add_issues.md),
[`ojo_add_counts()`](https://openjusticeok.github.io/ojodb/reference/ojo_add_counts.md),
[`ojo_add_minutes()`](https://openjusticeok.github.io/ojodb/reference/ojo_add_minutes.md),
[`ojo_add_party_details()`](https://openjusticeok.github.io/ojodb/reference/ojo_add_party_details.md),
[`ojo_check_ssl()`](https://openjusticeok.github.io/ojodb/reference/ojo_check_ssl.md))
accepts an optional `.con`. Because `.con` is placed after `...`, it
must be named in full — `ojo_tbl("case", .con = con)`, never by
position.

Passing nothing keeps the old convenience, so most v2 call sites need no
change here.

### 5. Explicit SSL Configuration

SSL settings are no longer hardcoded. You must specify `ssl_mode` (one
of `"disable"`, `"require"`, `"verify-ca"`, `"verify-full"`) and, for
the verifying modes, paths to certificates.

``` r

db_config(
  ssl_mode = "verify-ca",
  ssl_root_cert = "~/.postgresql/ojodb/server-ca.pem",
  ssl_cert = "~/.postgresql/ojodb/client-cert.pem",
  ssl_key = "~/.postgresql/ojodb/client-key.pem"
)
```

v2 required those three files to exist at `~/.postgresql/ojodb/` and
refused to authenticate otherwise. v3 warns rather than stops, so a bad
path now surfaces as a connection error from the server instead of an
error from
[`ojo_auth()`](https://openjusticeok.github.io/ojodb/reference/ojo_auth.md).

If the certificate paths are relative, they are resolved against the
*current working directory* at connect time — which is why they can work
interactively but fail under a test runner that changes directory.
Absolute paths are safer.

### 6. Removed functions

| Removed | Use instead |
|:---|:---|
| `ojo_search_minutes()` | `ojo_tbl("minute")` + [`dplyr::filter()`](https://dplyr.tidyverse.org/reference/filter.html) + [`ojo_collect()`](https://openjusticeok.github.io/ojodb/reference/ojo_collect.md), or [`ojo_query()`](https://openjusticeok.github.io/ojodb/reference/ojo_query.md) for raw SQL |
| `ojo_env()` | Nothing. Connection state is no longer a package-level object. |
| `ojo_show_row()` | Build the OSCN docket URL yourself and call [`utils::browseURL()`](https://rdrr.io/r/utils/browseURL.html). |
| `tbl_from_gcs_arrow()`, `tbl_from_gcs_duckdb()` | `ojo_connect(db_config = db_config(driver = "duckdb"))`, then [`ojo_tbl()`](https://openjusticeok.github.io/ojodb/reference/ojo_tbl.md) |
| `tbl_from_rpostgres()` | [`ojo_tbl()`](https://openjusticeok.github.io/ojodb/reference/ojo_tbl.md) |

## Troubleshooting

**Error: “Missing required configuration”** \* Check your `.Renviron`
file
([`usethis::edit_r_environ()`](https://usethis.r-lib.org/reference/edit.html)).
\* Ensure variables are named correctly (e.g., `OJO_HOST`, not
`OJO_DB_HOST`). \*
[`ojo_auth()`](https://openjusticeok.github.io/ojodb/reference/ojo_auth.md)
specifically requires `driver`, `host`, `port`, `username`, `password`,
and `ssl_mode`. Passing only a host and credentials is not enough.

**Error: “SSL certificate files not found”** \* If `ssl_mode` is
“verify-ca” or “verify-full”, ensure the paths to your certificate files
are correct and the files exist. \* Try `ssl_mode = "require"` if you
cannot verify the server’s certificate.

**Connection fails after upgrade** \* Try re-running
`ojo_auth(..., .install = TRUE, .overwrite = TRUE)` to completely
refresh your configuration. The bare `ojo_auth(db_config = ...)` call is
session-only and will not survive the session.

**FATAL: connection requires a valid client certificate** \* The server
demanded a client certificate and none was sent. Usually a cert path
that does not resolve — check that `OJO_SSL_CERT` and `OJO_SSL_KEY`
point at real files from your *current* working directory, or use
absolute paths.

**`OJO_USER` is unset but `OJO_ADMIN_USER` is set** \* You are running
on a v2 `.Renviron`. v3 does not read the `ADMIN`/`DEFAULT` pairs.
Re-run
[`ojo_auth()`](https://openjusticeok.github.io/ojodb/reference/ojo_auth.md)
with `.install = TRUE, .overwrite = TRUE`.

**Error: “object ‘ojo_env’ not found”, “could not find function
‘ojo_search_minutes’”** \* These were removed in v3.0.0; see the removal
table in the migration guide above.
