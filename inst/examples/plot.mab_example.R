# Objects returned by `single_mab_simulation()` have a `mab` class.
# This class has a plot generic that has several minimal plots to examine
# the trial quickly
#
# These functions require ggplot2
if (requireNamespace("ggplot2", quietly = TRUE)) {
  # Loading Data and running a quick simulation
  data(tanf)
  x <- single_mab_simulation(
    data = tanf,
    algorithm = "Thompson",
    assignment_method = "Batch",
    period_length = 600,
    whole_experiment = TRUE,
    perfect_assignment = TRUE,
    blocking = FALSE,
    prior_periods = "All",
    conditions = c(
      "no_letter",
      "open_appt",
      "specific_appt"
    ),
    data_cols = c(
      condition_col = "condition",
      id_col = "ic_case_id",
      success_col = "success"
    )
  )

  # View best treatment arms over the simulation
  y <- plot(x, type = "arm")
  y
  # Adding a new title
  y + ggplot2::labs(title = "Your New Title")
  # type = assign creates a similar plot, but shows probability of assignment instead


  # Plotting Augmented Inverse Probability Estimates with confidence interval
  # By default it provides 95% Normal Confidence Intervals but this can be adjusted
  plot(x, type = "estimate", estimator = "AIPW")

  # Adjusting height of internal geom* argument. (`geom_errorbarh()`)
  plot(x, type = "estimate", estimator = "AIPW", height = 0.4)
}
