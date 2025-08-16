# Objects returned by `multiple_mab_simulation()` have a `multiple.mab` class.
# This class has a summary generic that can produce quick results of the trials
data(tanf)
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

# Creating summary table
## Defaults to 95% confidence interval
summary(x) |> print(width = Inf)

## 70% confidence level
summary(x, level = 0.7) |> print(width = Inf)
