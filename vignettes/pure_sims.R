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
save.image("vignettes/pure_sims.RData")
