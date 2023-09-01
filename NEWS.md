# ojodb 2.6.1

## Bug Fixes
- Fixed a bug related to the `rlang` package.
- Removed `renv` from development environment in favor of package management using the DESCRIPTION file. This will only affect the development of the package going forward, and shouldn't affect analysts' use of the package.

# ojodb 2.6.0

## Aesthetics
- A fun loading message will now appear when the package is loaded. This can be disabled by setting `OJO_LOAD_MESSAGE=FALSE` in your local R environment.
- The `{ojodb}` package now has its own tidyverse-compliant sticker design.

## Package Maintenance
- The `ojo_list_` functions (`ojo_list_schemas()`, `ojo_list_tables()`, and `ojo_list_vars()`) now return a sorted list instead of an unsorted one.
- Documentation and namespacing errors (e.g. `{dplyr}` functions missing an explicit `.data` reference, missing namespace prefixes, etc.) were corrected. Consequently, `devtools::check()` now passes with minimal warnings / notes.

# ojodb 2.5.0 (2023-04-18)

## Analyst Tools
- You can now use `ojo_collect` to `collect` but with a progress bar and helpful information.

## Package Maintenance
- Added `ojo_version` function to return the version of the package. Useful for debugging.

# ojodb 2.4.0 (2023-04-13)

## Package Maintenance
- Exposed the `ojo_env` function to easily access the package-level environment where database connections are stored.
- Changed the default behavior of `ojo_connect` to use `DBI::dbConnect()` instead of `pool::dbPool()`. Connection pools are still available by passing `pool = TRUE` to `ojo_connect`.

# ojodb 2.3.4 (2023-04-13)

## Analyst Tools
- You can now cancel queries immediately with `Ctrl + c` or `Cmd + c` in the console.

# ojodb 2.3.3 (2023-04-12)

## Package Maintenance
- Implemented schema scoping for `ojo_tbl` using `DBI::Id()` instead of `dbplyr::in_schema()` to match common practice.

# ojodb 2.3.2 (2023-03-28)

## Analyst Tools
- Allows lowercase inputs to the `district` argument of `ojo_civ_cases` and `ojo_crim_cases`
- You no longer need to import `{dplyr}` manually!
- Database connections are handled for you by default. You won't even know it's there.

## Package Maintenance
- Added Github Actions for automated package checks
- Added `{dplyr}` to `Depends` field of the package `Description`
- Overhauled database connection management using package-level environment scoping. See `ojo_connect`, `ojo_tbl`, and `ojo_query` for more information.
- Replaced `{odbc}` dependency with `{RPostgres}`

# ojodb 2.3.1 (2023-03-02)

## Package Maintenance
- Add SQL file to create database roles
- Bump `{renv}` version to 0.17.0
- Add `docs/` folder to `.gitignore`
- Add `dontrun` to examples

# ojodb 2.3.0 (2023-03-02)

## Analyst Tools
- Fixed the `ojo_list_vars` function

## Package Maintenance
- Removed direct class comparisons in favor of `inherits`
- Added documentation for `ojo_add_issues` function
- Removed improper data import and resulting hard dependency on `{readr}`

# ojodb 2.2.0 (2022-11-09)

## Analyst Tools
* Added the `ojo_county_population` function, which will return the population for Oklahoma counties for the given years, in a format ready to join to OJO tables.

## Package Maintenance
* Pruned as many dependencies as possible and moved many more to `Suggests`.
* Added `person()` syntax to `DESCRIPTION`.

# ojodb 2.1.0 (2022-05-03)

## Analyst Tools

* Added the function `ojo_fiscal_year` to return the fiscal year of a given Date.
* Removed `ojo_table` alias for consistency and transparency.

## Package Maintenance
* Added a `NEWS.md` file to track changes to the package.
* Changed file name of png for portability.
