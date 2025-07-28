# whatifbandit
<!-- badges: start -->
[![R-CMD-check](https://github.com/ryantmoore/whatifbandit/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ryantmoore/whatifbandit/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/ryantmoore/whatifbandit/graph/badge.svg)](https://app.codecov.io/gh/ryantmoore/whatifbandit)
<!-- badges: end -->


## Overview
whatifbandit is a package designed to answer: "*what if my experiment was a bandit trial?*"

Using data from the original trial and the user specified options, it re simulates
a randomized controlled trial as an adaptive experiment. Augmented Inverse Probability Weighted estimation (AIPW)
following the work of [Hadad et. al (2021)](https://pubmed.ncbi.nlm.nih.gov/33876748/) to robustly estimate the probability of success under
the adaptive experiment.

Adaptive experimental designs tend to shift more participants over time to more promising treatment arms, which are more efficient at honing in on the best treatment. These designs can shine in situations such as:

-   Simple random assignment produces sample sizes too small to detect treatment effects.
-   A researcher wants to test a many treatments.
-   An experiment occurs over a long period of time.
-   Finding the absolute best treatment takes precedence over gauging the effects of all treatments.

This package allows researchers to showcase how their experiment could have turned out, without having the go through the process
of running another one. This can unlock new insights in older studies, and provide justification for the future usage of adaptive 
techniques.

Adaptive experiments are a Multi-Arm Bandit Problem, as each treatment arm has an unknown probability of success, and we are forced
to assign new participants/observations based on the outcomes already occurred, while attempting to balance finding more information on each
treatment, and exploiting the best treatment arm. This idea, is what gives the package its name, whatifbandit.

## Features
Whatifbandit provides robust customization options to match as many experimental designs as possible, but it is only 
equipped to handle experiments where success is binary. Functionality for other cases may be introduced in future development.
These major features include:

-   2 bandit algorithms probability matching via Thompson Sampling, and UCB1.
-   Variable length and flexible assignment periods, such as individual, batches, and date-based.
-   Simulation of perfect and imperfect information during re-assignment.
-   Treatment blocking.
-   Control augmented and hybrid assignment.
-   Variable length information period for re-assignment.

Additionally whatifbandit supports parallel processing over multiple simulations
via [future](https://future.futureverse.org/), large data support through [data.table](https://rdatatable.gitlab.io/data.table/),
and a full suite of generic functions to accelerate post simulation analysis.

## Installation
```
# Install From CRAN
install.packages("whatifbandit")

# Install Straight from GitHub
remotes::install_github("ryantmoore/whatifbandit")
```
## Usage 
```
# Running one trial.
sim <- single_mab_simulation(
  data = tanf,
  assignment_method = "Date",
  time_unit = "Month",
  period_length = 1,
  algorithm = "Thompson",
  conditions = levels(tanf$condition),
  whole_experiment = FALSE, perfect_assignment = TRUE,
  prior_periods = "All",
  blocking = TRUE, block_cols = c("service_center"),
  data_cols = c(
    id_col = "ic_case_id",
    date_col = "appt_date",
    success_col = "success",
    condition_col = "condition",
    month_col = "recert_month",
    success_date_col = "date_of_recert",
    assignment_date_col = "letter_sent_date"
  )
)

# Running multiple trials.
set.seed(532454)
seeds <- sample.int(1000000, 100, replace = FALSE)
multiple_sims <- multiple_mab_simulation(
    data = tanf,
    assignment_method = "Date",
    time_unit = "Month",
    period_length = 1,
    algorithm = "Thompson",
    conditions = levels(tanf$condition),
    whole_experiment = FALSE, perfect_assignment = TRUE,
    prior_periods = "All",
    blocking = TRUE, block_cols = c("service_center"),
    data_cols = c(
      id_col = "ic_case_id",
      date_col = "appt_date",
      success_col = "success",
      condition_col = "condition",
      month_col = "recert_month",
      success_date_col = "date_of_recert",
      assignment_date_col = "letter_sent_date"
    ),
    keep_data = FALSE, times = 100, seeds = seeds
  )
# Running in Parallel

future::plan("multisession", workers = availableCores()) # Choose an appropraite plan, and core count
set.seed(532454)
seeds <- sample.int(1000000, 100, replace = FALSE)
multiple_sims <- multiple_mab_simulation(
    data = tanf,
    assignment_method = "Date",
    time_unit = "Month",
    period_length = 1,
    algorithm = "Thompson",
    conditions = levels(tanf$condition),
    whole_experiment = FALSE, perfect_assignment = TRUE,
    prior_periods = "All",
    blocking = TRUE, block_cols = c("service_center"),
    data_cols = c(
      id_col = "ic_case_id",
      date_col = "appt_date",
      success_col = "success",
      condition_col = "condition",
      month_col = "recert_month",
      success_date_col = "date_of_recert",
      assignment_date_col = "letter_sent_date"
    ),
    keep_data = FALSE, times = 100, seeds = seeds
  )
future::plan("sequential")
```
## More Information
For more complete information about the package details, please refer to the vignette tutorial and the full documentation.

If you have any specific questions about the package, feel free to send me an at <noahochital@icloud.com>, and if you encounter
any bugs, please create an issue on [GitHub](https://github.com/ryantmoore/whatifbandit/issues) with a reproducible example.


