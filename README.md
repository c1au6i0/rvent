
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rvent

<!-- badges: start -->

[![](https://img.shields.io/badge/devel%20version-0.2.4.2000-blue.svg)](https://github.com/https://github.com/c1au6i0/rvent)
[![](https://img.shields.io/github/last-commit/c1au6i0/rvent.svg)](https://github.com/c1au6i0/rvent/commits/master)
<!-- badges: end -->

The goal of rvent is to summarize, analyze and plot experimental data
recorded with the [SCIREQ pletysmograph](https://www.scireq.com/).

The package was originally developed for being used by people with
limited knowledge of *R*. For example, when function parameter
`inter = TRUE`, dialog windows allow to interactively insert all others
required parameters.

The `rvent` packaged is used by a **the shiny app**
([link](https://rden.shinyapps.io/rvent_app/),
[github](https://github.com/c1au6i0/rvent_app)).

## Installation

``` r
# install.packages("devtools")

devtools::install_github("c1au6i0/rvent")
```

## Example

This is a basic example of how to use the package:

``` r
library(rvent)

# open dialog to select the directory with iox files to import
# import them and make them tidy
sess1 <- import_session() 

# open a series of interactive windows to select bin and baseline duration,
# missing info and drug treatments. Summarizes the data in an excel file
summarize_vent(sess1)

# for each subject creates a plot with 20 facets corresponding to the 20 recorded
# variables
session_plots(sess1)
```

You can bind tighter multiple excel-summary files into a single file by
moving them into the same folder and then running:

``` r
bind_summary()
```

This is the example plot generated by `session_plot` for a subject
Alex\_DeLarge that received milk-plus.

<img src="man/figures/README-plot-1.png" width="100%" />

### Accepted files and recording

The `rvent` package extract some of the metadata from the subject name
and comments provide in the output file. Thus, it is important to follow
some rules for recording sessions.

**—Subject name—**

*Subject name* needs to contain a numeric ID (rest of the name will be
discarded) followed by a drug name. These are all examples of accepted
formats for subject 1 receiving 3 mg/kg of milk:

    1 milk 3 mg/kg # prefered
    1_milk_3_mg/kg
    1 milk
    1 milk ignoredinfo

These are examples of not accepted formats:

    milk 1
    milk rat1

**—Comments—**

*Comments* are used to identify the injection time. When one or more
subjects receive an injection, use the SCIREQ software to add a comment
in the form “ID DRUG DOSE UNIT”. For example, these are the accepted
formats for a session in which subjects 1, 2 and 3 received 3 mg/kg of
milk:

    1 2 3 milk 3 mg/kg # prefered
    rat 1 and 2 and 3 milk 3 mg/kg
    rat1_2_3_milk 3 mg/kg
    rat1 2 3 milk

**If dose or unit are not inserted**, the package will ask you to fill
that info.

Note, that **subjects are expected to get one injection per session.**

For each subject, the time of the injection is identified by:

-   extracting rows with comments and columns relative to time, date and
    comments.
-   extracting from that dataframe, subject `ID DRUG DOSE UNIT`.
-   clean it up to have a single subject for each row.
-   inner-joining it with the dataframe of data by `ID` and `DATE`(this
    last one introduced in *v0.0.2.100*).

**This means that you can analyze together different sessions of the
same subject, as long as they are not from the same day.**

**—Example of iox file—**

This is an example of a [recording
file](https://1drv.ms/t/s!Am3aUTxhPMS8iM43UVMJXhO4mNCecw?e=b9gszt)
(containing randomly generated data).
