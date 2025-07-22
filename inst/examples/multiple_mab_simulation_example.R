# Multiple_mab_simulation() is a useful tool for running multiple trials
# using the same configuration settings, in different random states
data(tanf)
# Subsetting to make the example faster
tanf <- tanf[1:50, ]

# The seeds passed must be integers, so it is highly recommended to create them
# before using `sample.int()`
set.seed(1)
seeds <- sample.int(10000, 5)
conditions <- c("no_letter", "open_appt", "specific_appt")

# For this example, period_length is set a large interval and
# times is low to keep run time short.
start <- proc.time()
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
seq_time <- proc.time() - start
print(x)

# Its Recommenced to set keep_data at FALSE unless necessary to avoid
# the output from taking up to much memory
# Keep TRUE
object.size(x)
x$final_data_nest <- NULL
# Size if Keep was FALSE
object.size(x)

# multiple_mab_simulation() is implemented using furrr::future_map()
# so you can also run simulations in parallel using futures.
# Simply run your preferred plan and number of cores before multiple_mab_simulation.
# Like: 
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
# If your on Windows plan needs to be multisession
# If your on Unix (MacOS/Linux) you can use multicore or multisession
# If your running the code on a high performance cluster, look into
# using the future.batchtools API for whichever scheduler is used

# Check the future and furrr documentation for more details on possible options