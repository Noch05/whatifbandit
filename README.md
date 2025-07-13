# whatifbandit
<!-- badges: start -->
[![R-CMD-check](https://github.com/ryantmoore/whatifbandit/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ryantmoore/whatifbandit/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/ryantmoore/whatifbandit/graph/badge.svg)](https://app.codecov.io/gh/ryantmoore/whatifbandit)
<!-- badges: end -->


## Description
whatifbandit is a package for reconsidering randomized controlled trials
as adaptive experimental trials instead. Using information from the results
of an original experiment, it simulates the possible outcomes if an 
adaptive treatment assignment approach had been used instead. Augmented inverse
probability weighted estimates are used to robustly estimate the probability
of success under the adaptive experiment, and perform valid statistical inference.

whatifbandit provides robust customization options to accommodate any experiment
with binary successes/failures. It supports 2 Bandit algorithms, Thompson Sampling,
and UCB1, and a plethora of options to allow for the simulation of perfect/imperfect
information during assignment periods, stationary/non-stationary bandits,
treatment blocking, and control augmentation during assignment.

Additionally whatifbandit allows for parallel processing over multiple
simulations of the same trial, and large data support through `data.table`.

## Installation
```
# Install Straight from GitHub
remotes::install_github("ryantmoore/whatifbandit")
```


