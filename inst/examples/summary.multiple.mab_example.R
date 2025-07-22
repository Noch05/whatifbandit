# Objects returned by `multiple_mab_simulation()` have a `multiple.mab` class.
# This class has a summary generic that can produce quick results of the trials
data(tanf)
# Subsetting to make the example faster
tanf <- tanf[1:100, ]
# Simulating a few trials

seeds <- sample.int(10000, 5)
conditions <- c("no_letter", "open_appt", "specific_appt")
x <- multiple_mab_simulation(
  data = tanf,
  assignment_method = "Batch",
  period_length = 20,
  whole_experiment = TRUE,
  blocking = FALSE,
  perfect_assignment = TRUE,
  algorithm = "Thompson",
  prior_periods = "All",
  control_augment = 0,
  conditions = conditions,
  data_cols = c(
    condition_col = "condition",
    id_col = "ic_case_id",
    success_col = "success"
  ),
  verbose = FALSE, times = 5, seeds = seeds, keep_data = FALSE
)

# Calling `summary` Returns a summary table for the trial
# Upper and Lower Bounds default to 95% Confidence Intervals
summary(x) |>
  print(width = Inf) # calling width = Inf to so whole table prints

# We can also change the confidence level to anything between 0 and 1
# This only changes the upper and lower bounds that are presented.
summary(x, level = 0.7) |>
  dplyr::select(lower_normal:upper_empirical)

# Invalid levels throw an error
try(summary(x, level = 5))
