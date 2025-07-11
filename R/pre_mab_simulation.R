#' @name pre_mab_simulation
#' @inheritParams single_mab_simulation
#' @title Pre-Simulation Setup for [mab_simulation()]


#' @description Common function for all the actions that need to take place before
#' running the Multi-Arm-Bandit Simulation. Intakes data and column names,
#' check for valid arguments, prepare data and pre-compute key values.
#' @returns Named list containing:
#' \itemize{
#' \item `data_cols`: List of necessary columns in `data` as strings and symbols.
#' \item `block_cols`: List of columns to block by in `data` as strings and symbols.
#' \item `data`: Prepared `data` object the same class as inputted. Contains all
#' columns required for [mab_simulation()].
#' \item `imputation_information`: List containing necessary information
#' for outcome and date imputation for [mab_simulation()].
#' }
#'
#' @seealso
#' *[single_mab_simulation()]
#' *[multiple_mab_simulation()]
#' *[check_args()]
#' *[create_cutoff()]
#' *[create_new_cols()]
#' *[imputation_prep()]


pre_mab_simulation <- function(data,
                               assignment_method,
                               algorithm,
                               conditions,
                               prior_periods,
                               perfect_assignment,
                               whole_experiment,
                               blocking,
                               data_cols,
                               control_augment,
                               time_unit,
                               period_length,
                               block_cols,
                               verbose) {
  if (inherits(data, "data.table")) {
    data <- data.table::copy(data)
  } else {
    data <- tibble::as_tibble(data)
  }

  data_cols <- purrr::map(data_cols, ~ list(
    name = .x, symbol = rlang::sym(.x)
  )) |>
    stats::setNames(names(data_cols))

  if (!base::is.null(block_cols)) {
    block_cols <- list(name = block_cols, symbol = rlang::syms(block_cols))
  }

  # Input Validation
  check_args(
    data = data, time_unit = time_unit,
    perfect_assignment = perfect_assignment,
    algorithm = algorithm, period_length = period_length,
    whole_experiment = whole_experiment, prior_periods = prior_periods,
    data_cols = data_cols, block_cols = block_cols, conditions = conditions, blocking = blocking,
    assignment_method = assignment_method, verbose = verbose,
    control_augment = control_augment
  )

  # Preparing Data to be simulated
  verbose_log(verbose, "Preparing Data")

  data <- create_cutoff(
    data = data,
    data_cols = data_cols,
    period_length = period_length,
    assignment_method = assignment_method,
    time_unit = time_unit
  ) |>
    create_new_cols(
      data_cols = data_cols,
      perfect_assignment = perfect_assignment,
      blocking = blocking,
      block_cols = block_cols
    )
  # Pre-computing Important values to be accessed for the simulation
  verbose_log(verbose, "Precomputing")

  imputation_information <- imputation_prep(
    data = data,
    whole_experiment = whole_experiment,
    data_cols = data_cols,
    perfect_assignment = perfect_assignment
  )

  return(list(
    data_cols = data_cols,
    block_cols = block_cols,
    data = data,
    imputation_information = imputation_information
  ))
}
