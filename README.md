# whatifbandit
<!-- badges: start -->
[![R-CMD-check](https://github.com/Noch05/whatifbandit/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Noch05/whatifbandit/actions/workflows/R-CMD-check.yaml)
[![codecov](https://codecov.io/gh/Noch05/whatifbandit/branch/main/graph/badge.svg?token=B51SYMH66I)](https://app.codecov.io/gh/Noch05/whatifbandit)
<!-- badges: end -->


## Overview
whatifbandit is a package designed to answer: "*What if my experiment was a bandit trial?*"

Using data from the original trial and the user-specified options, it resimulates
a randomized controlled trial as an adaptive experiment. Augmented Inverse Probability Weighted estimation (AIPW) is used,
following the work of [Hadad et. al (2021)](https://pubmed.ncbi.nlm.nih.gov/33876748/), to robustly estimate the probability of success under
the adaptive experiment.

Adaptive experimental designs, instead of randomly assigning treatments, take into account the relative performance
of each treatment. Usually, this means that better-performing treatments will be assigned more participants at each assignment
period, allowing for a convergence to the best treatment arm. These designs can shine in situations such as:

-   Simple random assignment produces sample sizes too small to detect treatment effects.
-   A researcher wants to test many treatments, such as 10, 20, or 100.
-   An experiment occurs over a long time, and a researcher wants prior results to impact future assignments.
-   Finding the absolute best treatment takes precedence over gauging the effects of all treatments.

This package allows researchers to showcase how their experiment could have turned out using
adaptive assignment, without having the go through the process of running another one.
This can unlock new insights in older studies and justify the future usage of adaptive
techniques.

Adaptive experiments are a Multi-Arm Bandit Problem, as each treatment arm has an unknown probability of success, and we are forced
to assign new participants/observations based on the outcomes already occurred, while balancing the trade-off that comes with exploring
each arm, and exploiting the current best arm.

These ideas is what gives the package its name, whatifbandit. bandit for Multi-Arm-Bandit, and whatif for the central question
that the package answers.

## Features
Whatifbandit provides robust customization options to match as many experimental designs as possible, but it is only
equipped to handle experiments where success is binary. Functionality for other cases may be introduced in future development.
Some of our major features are:

-   2 bandit algorithms: probability matching via Thompson sampling, and UCB1.
-   Variable length and flexible assignment periods, such as individual, batch, and date-based.
-   Simulation of perfect and imperfect information during re-assignment.
-   Block randomized and cluster randomized designs.
-   Control augmented and hybrid assignments.
-   Variable length information period for re-assignment.

Additionally, whatifbandit supports parallel processing over multiple simulations
via [future](https://future.futureverse.org/), large data support through [data.table](https://rdatatable.gitlab.io/data.table/).

## Installation
```r
# Install the latest stable version from GitHub
remotes::install_github("Noch05/whatifbandit@v1.0.0")

# Install from v0.03.0 from CRAN
install.packages("whatifbandit")

```
## Usage
### Running 1 Trial
```r
sim <- mab_from_rct(
  success ~ condition,
  data = tanf,
  algorithm = "ucb1",
  period_method = "batch",
  period_length = 1000,
  whole_experiment = TRUE
)
```
## Running multiple trials.
```r
# Setting seed for Reproducible RNG
simulations <- mab_from_rct(
  success ~ condition + block(service_center),
  data = tanf,
  algorithm = "thompson",
  period_method = "date",
  time_unit = "month",
  period_length = 1,
  delayed_feedback = TRUE,
  assignment_date_col = letter_sent_date,
  success_date_col = date_of_recert,
  date_col = appt_date,
  month_col = recert_month,
  whole_experiment = FALSE,
  keep_data = TRUE,
  r = 100,
  seed = 532454
)
```
## Running in Multiple Trials in Parallel
```r
library(future)

# Set any arbitrary plan
future::plan(plan, workers = availableCores())

simulations <- mab_from_rct(
  success ~ condition + block(service_center),
  data = tanf,
  algorithm = "thompson",
  period_method = "date",
  time_unit = "month",
  period_length = 1,
  delayed_feedback = TRUE,
  assignment_date_col = letter_sent_date,
  success_date_col = date_of_recert,
  date_col = appt_date,
  month_col = recert_month,
  whole_experiment = FALSE,
  keep_data = TRUE,
  r = 100,
  seed = 532454
)
future::plan(sequential)
```
## More Information
For more complete information about the package details, please refer to the the full documentation.

If you have any specific questions about the package, feel free to send me an email at <noahochital@icloud.com>, and if you encounter
any bugs, please create an issue on [GitHub](https://github.com/Noch05/whatifbandit/issues) with a reproducible example.
