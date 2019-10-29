
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rvent

<!-- badges: start -->

[![](https://img.shields.io/badge/devel%20version-0.0.0.9000-blue.svg)](https://github.com/https://github.com/c1au6i0/rvent)
[![](https://travis-ci.org/c1au6i0/rvent.svg?branch=master)](https://travis-ci.org/c1au6i0/rvent)
[![](https://img.shields.io/github/last-commit/c1au6i0/rvent.svg)](https://github.com/c1au6i0/rvent/commits/master)
<!-- badges: end -->

The goal of rvent is to summarize, analyze and plot experimental data
recorded with the [SCIREQ pletysmograph](https://www.scireq.com/).

The package is meant to be used by user with limited knoledge of *R*
and, eventually, to be the foundation for an interactive shiny app.

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
# missing info regarding drug treatments and summarize the data
# in an excel file
summarize_vent(sess1)

# for each subject create a plot with 20 facets corresponding to the 20 recorded
# variables
session_plots(sess1, svDialogs::dlg_dir()$dir)
```

## Summary Excel file

This is how a summary excel file looks. Bin is 30 minute, subjects are
Alex\_DeLarge and Georgie and the drug administered is milk\_plus.

<img src="man/figures/excel.png" width="100%" />

## Plot

This is the plot for the subject Alex\_DeLarge that received milk-plus.

<img src="man/figures/README-plot-1.png" width="100%" />
