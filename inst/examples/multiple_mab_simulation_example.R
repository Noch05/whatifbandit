# Multiple_mab_simulation() is a useful tool for running multiple trials
# using the same configuration settings, in different random states
#
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
    id_col = "id",
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



# You can also run simulations in parallel using future
# Check out future documentation for details, but if on Linux or MacOS
# you probably should set future::plan("multicore") instead
start <- proc.time()
future::plan("multisession", workers = 2)
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
    id_col = "id",
    success_col = "success"
  ),
  verbose = FALSE, times = 5, seeds = seeds, keep_data = FALSE
)
parallel_time <- proc.time() - start
future::plan("sequential")
# Here it runs slower, but with larger datasets or more simulations
# parallel becomes useful at reducing compute time if the memory is available
# Sequential
seq_time
# Parallel
parallel_time
