# whatifbandit
<!-- badges: start -->
[![R-CMD-check](https://github.com/ryantmoore/whatifbandit/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ryantmoore/whatifbandit/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/ryantmoore/whatifbandit/graph/badge.svg)](https://app.codecov.io/gh/ryantmoore/whatifbandit)
<!-- badges: end -->


## Description
whatifbandit is a package designed to answer: "*what if my experiment was a bandit trial?*"

It re-simulates randomized controlled trials as if they
were conducted as adaptive experiments. Using data from the original trial
and user specified options, it runs an adaptive trial simulation with new treatments,
and imputed outcomes when necessary. Augmented inverse
probability weighted estimates are used to robustly estimate the probability
of success under the adaptive experiment, and perform valid statistical inference.

We provide robust customization options to accommodate many forms of experiment,
as long as the outcomes are binary successes/failures. It supports 2 bandit 
algorithms, Thompson Sampling, and UCB1, stationary/non-stationary bandit approaches,
perfect/imperfect information during assignment, treatment blocking, control augmentation,
and various scheduling methods (i.e. in batches, by date, or individually).

Additionally whatifbandit allows for parallel processing over multiple
simulations of the same trial via `future`,
and large data support through `data.table`.

## Installation
```
# Install Straight from GitHub
remotes::install_github("ryantmoore/whatifbandit")
```


