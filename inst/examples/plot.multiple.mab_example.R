# Objects returned by `single_mab_simulation()` have a `mab` class.
# This class has a plot generic has several minimal plots to examine the trials
# quickly
#
#
data(tanf)
tanf <- tanf[1:20, ]
# Simulating a few trials

seeds <- sample.int(100, 5)
conditions <- as.character(unique(tanf$condition))
x <- multiple_mab_simulation(
  data = tanf,
  assignment_method = "Batch",
  period_length = 10,
  whole_experiment = TRUE,
  blocking = FALSE,
  perfect_assignment = TRUE,
  algorithm = "Thompson",
  prior_periods = "All",
  control_augment = 0,
  data_cols = c(
    condition_col = "condition",
    id_col = "ic_case_id",
    success_col = "success"
  ),
  verbose = FALSE,
  times = 5,
  seeds = seeds,
  keep_data = FALSE
)

# View number of times each treatment was the best.
plot(x, type = "summary")

# View a histogram of the AIPW estimates for each treatment.
plot(x, type = "hist", quantity = "estimate")

# Plotting AIPW confidence intervals using the empirical cdf, from the simulated
# trials.
plot(x, type = "estimate", cdf = "empirical")

# Changing the title, like any ggplot2 object.
plot(x, type = "summary") + ggplot2::labs(title = "Your New Title")

# Changing the bin width of the histograms.
plot(x, type = "hist", quantity = "assignment", geom = list(binwidth = 0.05))
