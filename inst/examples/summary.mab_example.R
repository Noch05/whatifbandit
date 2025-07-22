# Objects returned by `single_mab_simulation()` have a `mab` class.
# This class has a summary generic that can produce quick results of the trial.

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

# Calling `summary` Returns a summary table for the trial
# Defaults to 95% Normal Confidence Intervals
# for the Augmented Inverse Probability Estimates
summary(x)

# We can also change the confidence level to anything between 0 and 1
summary(x, level = 0.7)

# Invalid levels throw an error
try(summary(x, level = 5))
