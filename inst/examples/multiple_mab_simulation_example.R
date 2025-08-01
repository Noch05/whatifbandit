# Multiple_mab_simulation() is a useful tool for running multiple trials
# using the same configuration settings, in different random states
data(tanf)
tanf <- tanf[1:50, ]

# The seeds passed must be integers, so it is highly recommended to create them
# before using `sample.int()`
set.seed(1)
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
  verbose = FALSE, times = 5, seeds = seeds, keep_data = TRUE
)
print(x)

# Its Recommenced to set keep_data at FALSE unless necessary to avoid
# the output from taking up to much memory
# Keep TRUE
object.size(x)

x$final_data_nest <- NULL
# Size if Keep was FALSE
object.size(x)


## Parallel Execution using future: 
## Check the future and furrr documentation for more details on possible options
\dontrun{

  future::plan("multisession", workers = 6)
  multiple_mab_simulation(data = tanf,
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
  future::plan("sequential")
}

