# Using ojo_auth with Multiple Configuration Sources

The enhanced `ojo_auth()` function can now read configuration from multiple sources with a clear precedence order:

1. **Function arguments** (highest precedence)
2. **Direct config list**
3. **YAML configuration file**  
4. **Environment variables from .Renviron**
5. **System environment variables** (lowest precedence)

## Basic Usage

```r
# Traditional usage - all parameters as arguments
ojo_auth(
  host = "your-host.example.com",
  port = "5432", 
  username = "your_username",
  password = "your_password"
)
```

## Using Custom SSL Certificate Paths

```r
# Specify custom SSL certificate locations
ojo_auth(
  host = "your-host.example.com",
  port = "5432",
  username = "your_username", 
  password = "your_password",
  ssl_root_cert = "/custom/path/server-ca.pem",
  ssl_cert = "/custom/path/client-cert.pem",
  ssl_key = "/custom/path/client-key.pem",
  ssl_mode = "require"
)
```

## Using a Configuration File

Create a YAML file (e.g., `~/.ojo_config.yaml`):

```yaml
host: "your-host.example.com"
port: "5432"
username: "your_username"
password: "your_password"
ssl_mode: "verify-ca"
ssl_root_cert: "/path/to/server-ca.pem"
ssl_cert: "/path/to/client-cert.pem"
ssl_key: "/path/to/client-key.pem"
```

Then use it:

```r
ojo_auth(config_file = "~/.ojo_config.yaml")
```

## Using a Configuration List

```r
config <- list(
  host = "your-host.example.com",
  port = "5432",
  username = "your_username",
  password = "your_password",
  ssl_mode = "verify-ca"
)

ojo_auth(config = config)
```

## Environment Variables

The function automatically reads from these environment variables:

- `OJO_HOST`
- `OJO_PORT` 
- `OJO_DEFAULT_USER` / `OJO_ADMIN_USER`
- `OJO_DEFAULT_PASS` / `OJO_ADMIN_PASS`
- `OJO_SSL_MODE`
- `OJO_SSL_ROOT_CERT`
- `OJO_SSL_CERT`
- `OJO_SSL_KEY`

## Mixing Configuration Sources

You can mix and match sources. Function arguments always take precedence:

```r
# Use config file for most settings, override specific values
ojo_auth(
  config_file = "~/.ojo_config.yaml",
  password = "new_password",  # Override password from config file
  ssl_mode = "require"        # Override SSL mode from config file
)
```

## Temporary Session Setup

For temporary use without modifying .Renviron:

```r
ojo_auth(
  host = "your-host.example.com",
  port = "5432",
  username = "your_username",
  password = "your_password",
  .install = FALSE  # Don't modify .Renviron
)
```