
<!-- README.md is generated from README.Rmd. Please edit that file -->

# LCBC Questionnaires <img src="man/figures/hex.png" align="right" alt="" width="120" />

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Codecov test
coverage](https://codecov.io/gh/lcbc-uio/questionnaires/branch/master/graph/badge.svg)](https://codecov.io/gh/lcbc-uio/questionnaires?branch=master)
[![R-CMD-check](https://github.com/lcbc-uio/questionnaires/workflows/R-CMD-check/badge.svg)](https://github.com/lcbc-uio/questionnaires/actions)
[![r-universe](https://lcbc-uio.r-universe.dev/badges/Questionnaires)](https://lcbc-uio.github.io/Questionnaires/)
<!-- badges: end -->

The repository contains functions to run conversions and calculate
components from commonly used questionnaires in LCBC research.

All functions in this package are prefixed with the name of the
questionnaire the function is intended for (i.e. `psqi_`, `ipaq_` etc.).
Column specifications may be manually input’ed, but if columns are named
after as the function expects (i.e. MOAS standard), the functions
generally work without manual input. The functions that will run all
(most) necessary steps to completely calculate components and sums are
named as `questionnaire_compute` (i.e. `psqi_compute()`, `ipaq_compute`,
etc.). These functions all have the option to `keep_all` which takes a
`TRUE` or `FALSE` statement on whether the data should be appended to
the input data, or just to return the computed columns.

Full documentation with tutorials and function docs online at
<https://lcbc-uio.github.io/questionnaires/>

## Installation

Install the package from the LCBC R-universe:

``` r
# Enable universe(s) by lcbc-uio
options(repos = c(
  lcbcuio = 'https://lcbc-uio.r-universe.dev',
  CRAN = 'https://cloud.r-project.org'))

# Install some packages
install.packages('lcbcr')
```

OR using the the package `remotes` to install.

``` r
# install.packages("remotes")
remotes::install_github("lcbc-uio/questionnaires")
```

## Questionnaire descriptions

### [Pittsburgh Sleep Quality Inventory](https://lcbc-uio.github.io/questionnaires/articles/psqi.html)

Despite the prevalence of sleep complaints among psychiatric patients,
few questionnaires have been specifically designed to measure sleep
quality in clinical populations. The Pittsburgh Sleep Quality Index
(PSQI) is a self-rated questionnaire which assesses sleep quality and
disturbances over a 1-month time interval. Nineteen individual items
generate seven “component” scores: subjective sleep quality, sleep
latency, sleep duration, habitual sleep efficiency, sleep disturbances,
use of sleeping medication, and daytime dysfunction. The sum of scores
for these seven components yields one global score.

### [International Physical Activity Questionnaire](https://lcbc-uio.github.io/questionnaires/articles/ipaq.html)

The purpose of the International Physical Activity questionnaires (IPAQ)
is to provide a set of well-developed instruments that can be used
internationally to obtain comparable estimates of physical activity.
There are two versions of the questionnaire. The short version is
suitable for use in national and regional surveillance systems and the
long version provide more detailed information often required in
research work or for evaluation purposes.

### [Edinburgh Handedness Inventory](https://lcbc-uio.github.io/questionnaires/articles/ehi.html)

The Edinburgh Handedness Inventory is a measurement scale used to assess
the dominance of a person’s right or left hand in everyday activities,
sometimes referred to as laterality. The inventory can be used by an
observer assessing the person, or by a person self-reporting hand use.
The latter method tends to be less reliable due to a person
over-attributing tasks to the dominant hand.

### [Beck Depression Inventory](https://lcbc-uio.github.io/questionnaires/articles/bdi.html)

Beck Depression Inventory-II (BDI-II) is one of the most widely used
instruments for measuring the severity of self-reported depression in
adolescents and adults. As a general rule, BDI-II is administrated in
LCBC to adults with an upper cut off around 60 years, while depression
in older adults is assessed with the Geriatric Depression Scale (GDS).
However, please consult the instructions for each project, as this
guideline has been implemented at different time points across the
projects.

### [Geriatric Depression Scale](https://lcbc-uio.github.io/questionnaires/articles/gds.html)

The Geriatric Depression Scale (GDS) is an instrument designed
specifically for rating depression in the elderly. It can be
administrated to healthy, medically ill, and mild to moderately
cognitively impaired older adults. As a general rule, GDS is
administrated in LCBC to older adults with a lower cut off around 60
years. However, please consult the instructions for each project, as
this guideline has been implemented at different time points across the
projects.

The questionnaire consists of 30 questions tapping into a wide variety
of topics relevant to depression, including cognitive complaints,
motivation, thoughts about the past and the future, self-image, and mood
itself. The answers should be based the participants’ feelings
throughout the last week.

Twenty of the questions indicate the presence of depression when
answered positively, while the ten remaining indicate depression when
answered negatively (see scoring instructions below). The questionnaire
is scored accordingly, giving one point for each statement that affirms
a depressive symptom. The sum of these scores yields one total score,
with a possible range between 0 and 30.
