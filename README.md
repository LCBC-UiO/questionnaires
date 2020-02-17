
<!-- README.md is generated from README.Rmd. Please edit that file -->

# LCBC Questionnaires <img src="man/figures/hex.png" align="right" alt="" width="120" />

<!-- badges: start -->

[![CircleCI build
status](https://circleci.com/gh/LCBC-UiO/Questionnaires.svg?style=svg&circle-token=8de4dd4d0d428ed1382feef5513cfa15aac3703e)](https://circleci.com/gh/LCBC-UiO/Questionnaires)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Travis build
status](https://travis-ci.org/LCBC-UiO/Questionnaires.svg?branch=master)](https://travis-ci.org/LCBC-UiO/Questionnaires)
[![Codecov test
coverage](https://codecov.io/gh/LCBC-UiO/Questionnaires/branch/master/graph/badge.svg)](https://codecov.io/gh/LCBC-UiO/Questionnaires?branch=master)
<!-- badges: end -->

The repository contains functions to run coversions and calculate
components from commonly used questionnaires in LCBC research.

The questionnaires covered so far:

  - PSQI - [Pittsburgh Sleep Quality
    Inventory](https://lcbc-uio.github.io/Questionnaires/articles/psqi.html)
  - IPAQ - [International Physical Activity
    Questionnaire](https://lcbc-uio.github.io/Questionnaires/articles/ipaq.html)
  - EHI - [Edinburgh Handedness
    Inventory](https://lcbc-uio.github.io/Questionnaires/articles/ehi.html)
  - BDI - [Beck Depression
    Inventory](https://lcbc-uio.github.io/Questionnaires/articles/bdi.html)
  - GDS - [Geriatric Depression
    Scale](https://lcbc-uio.github.io/Questionnaires/articles/gds.html)
  - EDU - [Education
    questions](https://lcbc-uio.github.io/Questionnaires/articles/edu.html)
  - TAS - \[The Toronto Alexithymia Scale (not documented yet)\]

All functions in this package are prefixed with the name of the
questionnaire the function is intended for (i.e. `psqi_`, `ipaq_` etc.).
Column specifications may be manually inputed, but if columns are named
after as the function expects (i.e. MOAS standard), the functions
generally work without manual input. The functions that will run all
(most) necessary steps to completely calculate components and sums are
named as `questionnaire_compute` (i.e. `psqi_compute()`, `ipaq_compute`,
etc.). These functions all have the option to `keep_all` which takes a
`TRUE` or `FALSE` statement on whether the data should be appended to
the input data, or just to return the computed columns.

Full documentation with tutorials and function docs online at
<https://lcbc-uio.github.io/Questionnaires/>

## Installation

This package is only on github, and requires the package `remotes` to
install.

``` r
# install.packages("remotes")
remotes::install_github("LCBC-UiO/Questionnaires")
```
