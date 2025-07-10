# Objects returned by `single_mab_simulation()` have a `mab` class.
# This class has a plot generic has several minimal plots to examine the trials
# quickly
#
# # These functions require ggplot2
if (requireNamespace("ggplot2", quietly = TRUE)) {
  data(tanf)
  # Subsetting to make the example faster
  tanf <- tanf[1:20, ]
  # Simulating a few trials

  seeds <- sample.int(100, 5)
  conditions <- c("no_letter", "open_appt", "specific_appt")
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
    conditions = conditions,
    data_cols = c(
      condition_col = "condition",
      id_col = "id",
      success_col = "success"
    ),
    verbose = FALSE, times = 5, seeds = seeds, keep_data = FALSE
  )

  # The plot generic has several options
  # Specify type = summary, to get a bar graph showing each time
  # a treatment group was selected as the best.
  plot(x, type = "summary")

  # type = hist, creates a histogram of AIPW, Sample, or Both estimates for each
  # treatment over each trial
  plot(x, type = "hist", estimator = "AIPW")

  # type = estimate creates a similar error bar plot like in plot.mab()
  # but here the empirical variance of the estimate can be used instead
  plot(x, type = "estimate", estimator = "AIPW", cdf = "empirical")

  # These plots can be added to like any ggplot2 object
  plot(x, type = "summary") + ggplot2::labs(title = "Your New Title")

  # Each only uses 1 geom, so arguments for them can be added in the function call
  plot(x, type = "hist", estimator = "AIPW", binwidth = 0.05)
}
