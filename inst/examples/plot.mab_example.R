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

  # The plot generic has several options
  # specify type = arm, to plot the Thompson probabilities or UCB1 statistics
  # over the treatment periods of the trial
  y <- plot(x, type = "arm")
  y
  # These can be added to like any ggplot2 object

  y + ggplot2::labs(title = "Your New Title")

  # type = assign creates a similar plot, but its only useful when
  # control_augmentation is > 0.


  # Setting type = estiamte, allows for plotting of the
  # Augmented Inverse Probability Estimates.
  # By default it provides 95% Normal Confidence Intervals but this can be adjusted
  plot(x, type = "estimate", estimator = "AIPW")

  # Each type only uses 1 ggplot2 geom* so any arguments for the particular geom
  # can be added into the generic call
  # Changing the height for `geom_errorbarh`
  plot(x, type = "estimate", estimator = "AIPW", height = 0.4)
}
