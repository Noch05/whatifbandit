#' @name pre_mab_simulation
#' @inheritParams single_mab_simulation
#' @title Pre-Simulation Setup for an adaptive Multi-Arm-Bandit Trial

#' @description Common function for all the actions that need to take place before
#' running the Multi-Arm-Bandit simulation. Intakes the data and column names to
#' check for valid arguments, format and create new columns as needed, and pre-compute
#' key values to avoid doing so within the simulation loop.
#'
#' @returns Named list containing:
#' \itemize{
#' \item `data_cols`: List of necessary columns in `data` as strings and symbols.
#' \item `block_cols`: List of columns to block by in `data` as strings and symbols.
#' \item `data`: Prepared tibble/data.table containing all the necessary columns to
#' conduct the adaptive trial simulation.
#' columns required for [mab_simulation()].
#' \item `imputation_information`: List containing necessary information
#' for outcome and date imputation for [mab_simulation()].
#' }
#' @details
#' If a data.frame is passed, as input data it is internally converted into
#' a tibble. If a data.table is passed it is copied to avoid modifying the
#' original dataset in the users environment.

#'
#' @seealso
#' *[single_mab_simulation()]
#' *[multiple_mab_simulation()]
#' @keywords internal


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
  if (base::is.null(data) || !base::is.data.frame(data)) {
    rlang::abort("Input 'data' must be a non-null data.frame.")
  }
  if (inherits(data, "data.table")) {
    data <- data.table::copy(data)
  } else if (!inherits(data, "tbl_df")) {
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

  imputation_information <- imputation_precompute(
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
#------------------------------------------------------------------------------
#' @title Simulates Multi-Arm Bandit Trial From Prepared Inputs
#' @name mab_simulation
#'
#' @description Internal helper to [single_mab_simulation()]
#' and [multiple_mab_simulation()]. Centralizes necessary functions to conduct a
#' single Multi-Arm-Bandit Trial with adaptive inference. It assumes all inputs have
#' been preprocessed by [pre_mab_simulation()].
#' @inheritParams single_mab_simulation
#' @inheritParams run_mab_trial
#'
#' @returns:
#' \itemize{
#' \item `final_data`: The processed tibble or data.table, containing new columns pertaining to the results of the trial.
#' \item `bandits`: A tibble or data.table containing the UCB1 statistics or Thompson Sampling posterior distributions for each period.
#' \item `assignment_probs`: A tibble or data.table containing the probability of being assigned each treatment arm at a given period.
#' \item `estimates`: A tibble or data.table containing the
#' AIPW (Augmented Inverse Probability Weighting) treatment effect estimates and variances, and traditional
#' sample means and variances, for each treatment arm.
#' \item `settings`: A named list of the configuration settings used in the trial.
#' }
#' @seealso
#'* [single_mab_simulation()]
#'* [multiple_mab_simulation()]
#' @keywords internal
#'


mab_simulation <- function(data,
                           time_unit,
                           perfect_assignment,
                           algorithm,
                           period_length,
                           prior_periods,
                           whole_experiment,
                           conditions,
                           blocking,
                           block_cols,
                           data_cols,
                           verbose,
                           assignment_method, control_augment,
                           imputation_information) {
  sim_results <- run_mab_trial(
    data = data,
    time_unit = time_unit,
    period_length = period_length,
    prior_periods = prior_periods,
    algorithm = algorithm,
    whole_experiment = whole_experiment,
    perfect_assignment = perfect_assignment,
    conditions = conditions,
    blocking = blocking,
    block_cols = block_cols,
    data_cols = data_cols,
    verbose = verbose,
    control_augment = control_augment,
    imputation_information = imputation_information
  )
  periods <- base::max(sim_results$final_data$period_number)

  sim_results$final_data <- get_iaipw(
    data = sim_results$final_data,
    assignment_probs = sim_results$assignment_probs,
    conditions = conditions,
    periods = periods,
    verbose = verbose
  )
  estimates <- adaptive_aipw(
    data = sim_results$final_data,
    assignment_probs = sim_results$assignment_probss,
    periods = periods,
    conditions = conditions,
    verbose = verbose
  )

  results <- list(
    final_data = sim_results$final_data,
    bandits = sim_results$bandits,
    assignment_probs = sim_results$assignment_probs,
    estimates = estimates,
    settings = list(
      data = NULL,
      assignment_method = assignment_method,
      control_augment = control_augment,
      time_unit = time_unit,
      perfect_assignment = perfect_assignment,
      algorithm = algorithm,
      period_length = period_length,
      prior_periods = prior_periods,
      whole_experiment = whole_experiment,
      conditions = conditions,
      blocking = blocking,
      block_cols = block_cols$name
    )
  )

  class(results) <- c("mab", class(results))

  return(results)
}
