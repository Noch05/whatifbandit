# Objects returned by `single_mab_simulation()` have a `mab` class.
# This comes a summary generic that can produce quick results of the trial.

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
    id_col = "id",
    success_col = "success"
  )
)

# Defaults to 95% Normal Confidence Intervals
# for the Augmented Inverse Probability Estimates
summary(x)

# We can also change the confidence level to anything valid
summary(x, level = 0.7)

# Invalid levels throw an error
try(summary(x, level = 5))
