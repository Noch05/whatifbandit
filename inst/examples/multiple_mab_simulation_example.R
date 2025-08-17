# Multiple_mab_simulation() is a useful tool for running multiple trials
# using the same configuration settings, in different random states
data(tanf)
tanf <- tanf[1:50, ]

# The seeds passed must be integers, so it is highly recommended to create them
# before using `sample.int()`
seeds <- sample.int(10000, 5)
conditions <- c("no_letter", "open_appt", "specific_appt")

## Sequential Execution
x <- multiple_mab_simulation(
  data = tanf,
  assignment_method = "Batch",
  period_length = 25,
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
print(x)

## Parallel Execution using future:
## Check the future and furrr documentation for more details on possible options
if (requireNamespace("future", quietly = TRUE)) {
    # Set a Proper "plan"
    \donttest{future::plan("multisession", workers = 2)}
    multiple_mab_simulation(
      data = tanf,
      assignment_method = "Batch",
      period_length = 25,
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
      verbose = FALSE, times = 5, seeds = seeds, keep_data = TRUE
    )
    # Always Set back to sequential to close processes
   \donttest{future::plan("sequential")}
}
