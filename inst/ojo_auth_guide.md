# OJO Authentication Guide

The `ojodb` package provides a clean, flexible authentication system using two main functions:

- `db_config()` - Creates database configuration objects
- `ojo_auth()` - Sets up authentication using a db_config object

## Basic Usage

### Creating Configuration

```r
# Method 1: Explicit parameters
config <- db_config(
  host = "your-host.com",
  port = "5432", 
  username = "your_username",
  password = "your_password"
)

# Method 2: From YAML file
config <- db_config(config_file = "~/.ojo_config.yaml")

# Method 3: From environment variables with admin credentials
config <- db_config(.admin = TRUE)
```

### Setting Up Authentication

```r
# Install configuration to .Renviron (persistent)
ojo_auth(config)

# Use for current session only
ojo_auth(config, .install = FALSE)

# One-liner usage
ojo_auth(db_config(host = "localhost", port = "5432", username = "user", password = "pass"))
```

## Configuration Sources

The `db_config()` function reads configuration from multiple sources with this precedence:

1. **Function arguments** (highest priority)
2. **YAML configuration file**
3. **Environment variables** (.Renviron and system)

### Environment Variables

The system recognizes these environment variables:

- `OJO_HOST` - Database host
- `OJO_PORT` - Database port  
- `OJO_DEFAULT_USER` / `OJO_ADMIN_USER` - Username
- `OJO_DEFAULT_PASS` / `OJO_ADMIN_PASS` - Password
- `OJO_SSL_MODE` - SSL connection mode
- `OJO_SSL_ROOT_CERT` - SSL root certificate path
- `OJO_SSL_CERT` - SSL client certificate path
- `OJO_SSL_KEY` - SSL private key path

### YAML Configuration

Example `~/.ojo_config.yaml`:

```yaml
host: "your-database-host.com"
port: "5432"
username: "your_username" 
password: "your_password"

# SSL settings (optional)
ssl_mode: "verify-ca"
ssl_root_cert: "/path/to/server-ca.pem"
ssl_cert: "/path/to/client-cert.pem"
ssl_key: "/path/to/client-key.pem"
```

Nested format is also supported:

```yaml
ojo:
  host: "your-database-host.com"
  port: "5432"
  username: "your_username"
  password: "your_password"
```

## SSL Configuration

By default, SSL certificates are expected in `$HOME/.postgresql/ojodb/`:
- `server-ca.pem` (root certificate)
- `client-cert.pem` (client certificate)  
- `client-key.pem` (private key)

You can specify custom paths:

```r
config <- db_config(
  host = "localhost",
  port = "5432",
  username = "user",
  password = "pass",
  ssl_root_cert = "/custom/path/server-ca.pem",
  ssl_cert = "/custom/path/client-cert.pem", 
  ssl_key = "/custom/path/client-key.pem",
  ssl_mode = "require"
)
```

## Advanced Usage

### Mixing Configuration Sources

```r
# Start with environment variables, override password
config <- db_config(password = "new_password")

# Use YAML file, override specific settings
config <- db_config(
  config_file = "~/.ojo_config.yaml",
  ssl_mode = "require"
)
```

### Admin vs Default Users

The system automatically detects admin users (usernames containing "admin") and sets appropriate environment variables:

```r
# Creates admin configuration
admin_config <- db_config(username = "adminuser", password = "adminpass")
ojo_auth(admin_config)  # Sets OJO_ADMIN_USER/OJO_ADMIN_PASS

# Creates default user configuration  
user_config <- db_config(username = "normaluser", password = "userpass")
ojo_auth(user_config)   # Sets OJO_DEFAULT_USER/OJO_DEFAULT_PASS
```

## Migration from Legacy ojo_auth()

Old usage:
```r
ojo_auth(host = "localhost", port = "5432", username = "user", password = "pass")
```

New equivalent:
```r
ojo_auth(db_config(host = "localhost", port = "5432", username = "user", password = "pass"))
```

The new system provides much more flexibility while maintaining the same core functionality.