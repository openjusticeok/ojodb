---
output: github_document
---

# The Open Justice Oklahoma `{ojodb}` Package

`{ojodb}` is a package that assists [Open Justice Oklahoma](https://openjustice.okpolicy.org/){target="_blank"} analysts in accessing and analyzing court, jail, prison, and other data collected from various sources.

The pkgdown website for `{ojodb}` can be found [here](https://openjusticeok.github.io/ojodb/){target="_blank"}.

## Installation

Install the devtools package if you don't have it yet, then install `{ojodb}` from GitHub with:

    if (!"devtools" %in% installed.packages()) {
      install.packages("devtools")
    }

    devtools::install_github("openjusticeok/ojodb")

After installation, you can update it with the same commands.

## Purpose and Goals

The goal of Open Justice Oklahoma is to collect and analyze hard-to-access data in order to better understand our state's justice system. The backbone of our work is our database, which consists of administrative data, produced in court's daily activities, gathered mainly from courts, jails, and prisons across the state. The data is collected through a variety of methods including webscraping and database file downloads. The `{ojodb}` package was built to give analysts a way to access this data and analyze it using shared methodological standards.

Because the data we analyze is mostly administrative data generated for case-by-case uses, it is always messy and contains errors. OJO's work depends on our processes to work through and around the imperfections in order to extract useful information, while acknowledging the limitations of the data.

For some data sources, OJO processes periodically pull new data into our database. For example, we have OSCN scrapers set up to periodically visit small claims case pages like [this one](https://www.oscn.net/dockets/GetCaseInformation.aspx?db=tulsa&number=SC-2019-10){target="_blank"} every few days, gathering new data that appears in the course of a case.

![Example case](man/figures/case_example.png){width="110%"}

We're generally not interested in what happens in a single case, but rather in aggregate trends at the county or state level. In the case linked above, for instance, we see that the eviction case was filed on January 2, 2019, and granted by default judgment on January 11. We can use small claims data collected from OSCN and ODCR to figure out how many eviction cases are filed, resulted in a judgment, and dismissed. This is information that isn't compiled anywhere else, so our database gives us the unique opportunity to understand what is really happening, day by day and in close to real-time, in Oklahoma's justice system.

In addition to court data, which is available on OSCN in near-real time, we collect data from the Oklahoma Department of Corrections, the Tulsa County Jail, the Oklahoma County Jail, the Pardon and Parole Board, and other sources.

# Guides and Resources

Our documentation, like everything else, is a work in progress.

-   See `vignette("vignette-pulling-data")` on using `{ojodb}` to pull data from the Oklahoma State Court Network
-   [The general Open Justice Oklahoma documentation website](https://ojo-documentation-ie5mdr3jgq-uc.a.run.app/)
-   [The Open Justice Oklahoma public-facing website](https://openjustice.okpolicy.org/)
-   [The Oklahoma Policy Institute Stackoverflow Teams site](https://stackoverflowteams.com/c/oklahoma-policy-institute/questions)
