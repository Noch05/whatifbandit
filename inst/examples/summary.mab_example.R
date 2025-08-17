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
  data_cols = c(
    condition_col = "condition",
    id_col = "ic_case_id",
    success_col = "success"
  )
)

# Creating summary table
## Defaults to 95% confidence interval
summary(x) |> print(width = Inf)

## 70% confidence level
summary(x, level = 0.7) |> print(width = Inf)
