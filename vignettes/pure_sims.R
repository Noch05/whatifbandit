library(whatifbandit)
library(tidyverse)
data(tanf)

future::plan("multisession", workers = 6)
set.seed(53245)
pure_ucb_sims <- mab_from_rct(
  success ~ condition,
  data = tanf,
  algorithm = "ucb1",
  period_method = "individual",
  keep_data = TRUE,
  seed = TRUE,
  r = 100,
  whole_experiment = TRUE
)
pure_ts_sims <- update(pure_ucb_sims$config, algorithm = "thompson")
future::plan("sequential")


# Selecting only the elements used in the vignette to reduce size
set.seed(0934)
i <- sample.int(100, 1)

sims <- c("pure_ucb_sims", "pure_ts_sims")
sims <- lapply(mget(sims), \(x) {
  x$new_data <- x$new_data |>
    unnest(data) |>
    select(trial, period_number, mab_success, mab_condition) |>
    nest(data = c(period_number, mab_success, mab_condition))
  x$bandit$assignment_prob <- NULL
  x$bandit$statistic <- NULL
  x$means <- NULL
  x$f_stats <- NULL
  x$config <- NULL
  return(x)
})
list2env(sims, envir = .GlobalEnv)
rm(sims)
save.image("vignettes/pure_sims.RData", compress = "xz")
