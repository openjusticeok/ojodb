# OJO Collect

A wrapper for dplyr::collect() that includes a progress bar Pull a lazy
tibble into your local environment from the OJO database, using
[`ojo_crim_cases()`](https://openjusticeok.github.io/ojodb/reference/ojo_crim_cases.md),
[`ojo_civ_cases()`](https://openjusticeok.github.io/ojodb/reference/ojo_civ_cases.md),
etc.

## Usage

``` r
ojo_collect(data, silent = !rlang::is_interactive())
```

## Arguments

- data:

  The lazy tibble to be pulled

- silent:

  Should the command line interface elements be shown? Defaults to
  `TRUE` if the user is in an interactive session, and `FALSE` if they
  are not.

## Value

A local tibble

## Examples

``` r
if (FALSE) { # \dontrun{
ojo_crim_cases() |>
 head(100) |>
 ojo_collect()
} # }
```
