# whatifbandit
<!-- badges: start -->
[![R-CMD-check](https://github.com/Noch05/whatifbandit/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Noch05/whatifbandit/actions/workflows/R-CMD-check.yaml)
[![codecov](https://codecov.io/gh/Noch05/whatifbandit/branch/main/graph/badge.svg?token=B51SYMH66I)](https://codecov.io/gh/Noch05/whatifbandit)
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
These major features include:

-   2 bandit algorithms: probability matching via Thompson sampling, and UCB1.
-   Variable length and flexible assignment periods, such as individual, batch, and date-based.
-   Simulation of perfect and imperfect information during re-assignment.
-   Treatment blocking.
-   Control augmented and hybrid assignments.
-   Variable length information period for re-assignment.

Additionally, whatifbandit supports parallel processing over multiple simulations
via [future](https://future.futureverse.org/), large data support through [data.table](https://rdatatable.gitlab.io/data.table/),
and a full suite of generic functions to accelerate post-simulation analysis.

## Installation
```
# Install From CRAN
install.packages("whatifbandit")

# Install most recent version from GitHub
remotes::install_github("Noch05/whatifbandit")
```
## Usage 
### Running 1 Trial
```
sim <- single_mab_simulation(
  data = tanf,
  assignment_method = "Batch",
  period_length = 1000,
  algorithm = "UCB1",
  whole_experiment = TRUE, 
  perfect_assignment = TRUE,
  prior_periods = "All",
  blocking = FALSE,
  data_cols = c(
    id_col = "ic_case_id",
    success_col = "success",
    condition_col = "condition"
  )
)
```
## Running multiple trials.
```
# Setting seed for Reproducible RNG for the simulation seeds
set.seed(532454)
seeds <- sample.int(1000000, 100, replace = FALSE) 

multiple_sims <- multiple_mab_simulation(
    data = tanf,
    assignment_method = "Date",
    time_unit = "Month",
    period_length = 1,
    algorithm = "Thompson",
    whole_experiment = FALSE, 
    perfect_assignment = TRUE,
    prior_periods = "All",
    blocking = TRUE, 
    block_cols = c("service_center"),
    data_cols = c(
      id_col = "ic_case_id",
      date_col = "appt_date",
      success_col = "success",
      condition_col = "condition",
      month_col = "recert_month",
      success_date_col = "date_of_recert",
      assignment_date_col = "letter_sent_date"
    ),
    keep_data = TRUE, times = 100, seeds = seeds
  )
```
## Running in Multiple Trials in Parallel
```
future::plan("multisession", workers = availableCores()) # Choose an appropriate plan, and core count for your system

# Setting seed for Reproducible RNG for the simulation seeds
set.seed(532454)
seeds <- sample.int(1000000, 100, replace = FALSE) 


multiple_sims <- multiple_mab_simulation(
    data = tanf,
    assignment_method = "Date",
    time_unit = "Month",
    period_length = 1,
    algorithm = "Thompson",
    whole_experiment = FALSE, 
    perfect_assignment = TRUE,
    prior_periods = "All",
    blocking = TRUE, 
    block_cols = c("service_center"),
    data_cols = c(
      id_col = "ic_case_id",
      date_col = "appt_date",
      success_col = "success",
      condition_col = "condition",
      month_col = "recert_month",
      success_date_col = "date_of_recert",
      assignment_date_col = "letter_sent_date"
    ),
    keep_data = TRUE, times = 100, seeds = seeds
  )
future::plan("sequential")
```
## More Information
For more complete information about the package details, please refer to the vignette tutorial and the full documentation.

If you have any specific questions about the package, feel free to send me an email at <noahochital@icloud.com>, and if you encounter
any bugs, please create an issue on [GitHub](https://github.com/Noch05/whatifbandit/issues) with a reproducible example.


