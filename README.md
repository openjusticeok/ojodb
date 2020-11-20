
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ojodb

ojodb is a package that assists [Open Justice
Oklahoma](https://openjustice.okpolicy.org/) analysts to access and
analyze court, jail, prison, and other data collected from various
sources.

The pkgdown website for the ojodb package can be found
[here](https://openjusticeok.github.io/ojodb/).

## Installation

Install the devtools package if you don’t have it yet, then install the
ojodb package from GitHub with:

``` r
if (!"devtools" %in% installed.packages()) {
  install.packages("devtools")
}

devtools::install_github("openjusticeok/ojodb")
```

After installation, update it with:

``` r
ojodb::ojo_reinstall()
```

## The big picture

The goal of Open Justice Oklahoma is to collect and analyze
hard-to-access data in order to better understand our state’s justice
system. The backbone of our work is our database, which consists of
administrative data, produced in court’s daily activities, gathered
mainly from courts, jails, and prisons across the state. The data is
collected through a variety of methods including webscraping and
database file downloads. The ojodb package was built to give analysts a
way to access this data and analyze it using shared methodological
standards.

Because the data we analyze is mostly administrative data generated for
case-by-case uses, it is always messy and contains errors. OJO’s work
depends on our processes to work through and around the imperfections in
order to extract useful information, while acknowledging the limitations
of the data.

For some data sources, OJO processes periodically pull new data into our
database. For example, we have OSCN scrapers set up to periodically
visit small claims case pages like [this
one](https://www.oscn.net/dockets/GetCaseInformation.aspx?db=tulsa&number=SC-2019-10)
every few days, gathering new data that appears in the course of a case.

We’re generally not interested in what happens in a single case, but
rather in aggregate trends at the county or state level. In the case
linked above, for instance, we see that the eviction case was filed on
January 2, 2019, and granted by default judgment on January 11. We can
use small claims data collected from OSCN and ODCR to figure out how
many eviction cases are filed, resulted in a judgment, and dismissed.
This is information that isn’t compiled anywhere else, so our database
gives us the unique opportunity to understand what is really happening,
day by day and in close to real-time, in Oklahoma’s justice system.

In addition to court data, which is available on OSCN in near-real time,
we collect data from the Oklahoma Department of Corrections, the Tulsa
County Jail, Pardon and Parole Board, and other sources.

## Exploring the OJO database

We’ve collected tons and tons of data, and it can be overwhelming to
approach it at first. To list all tables in the OJO database, use
`ojo_list_tables()`.

``` r
library(ojodb)

connect_ojo()
t <- ojo_list_tables()
```

Each table name begins with a prefix indicating the source of the data.
The most commonly used data sources are listed below.

| Prefix        | Source                             | Link                                                    |
| :------------ | :--------------------------------- | :------------------------------------------------------ |
| dlm\_, dlm2\_ | Tulsa County Jail                  | <http://iic.tulsacounty.org/expInmateBookings/Reindex>  |
| doc\_         | Oklahoma Department of Corrections | <http://doc.ok.gov/odoc-public-inmate-data>             |
| odcr\_        | On Demand Court Records            | <https://www1.odcr.com/>                                |
| ojo\_         | Open Justice Oklahoma              | Tables containing data generated by OJO processes       |
| oscn\_        | Oklahoma State Court Network       | <https://www.oscn.net/dockets/search.aspx>              |
| ppb\_         | Oklahoma Pardon and Parole Board   | <https://www.ok.gov/ppb/Dockets_and_Results/index.html> |

To see the variables and first 10 rows of a table, call `ojo_tbl()` on
it:

``` r
ojo_tbl("oscn_crim_disps")
```

See the vignettes under the Articles tab for more information on our
database and how to use it.
