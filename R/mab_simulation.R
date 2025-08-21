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
#' If a data.frame is passed as input data it is internally converted into
#' a tibble. If a data.table is passed it is copied to avoid modifying the
#' original dataset in the users environment.

#'
#' @seealso
#' * [single_mab_simulation()]
#' * [multiple_mab_simulation()]
#' @keywords internal

pre_mab_simulation <- function(
  data,
  assignment_method,
  algorithm,
  control_condition,
  prior_periods,
  perfect_assignment,
  whole_experiment,
  blocking,
  data_cols,
  control_augment,
  time_unit,
  period_length,
  block_cols,
  verbose,
  ndraws,
  random_assign_prop,
  check_args
) {
  if (base::is.null(data) || !base::is.data.frame(data)) {
    rlang::abort("Input 'data' must be a non-null data.frame.")
  }
  if (data.table::is.data.table(data)) {
    data <- data.table::copy(data)
  } else if (!tibble::is_tibble(data)) {
    data <- tibble::as_tibble(data)
  }

  data_cols <- purrr::map(
    data_cols,
    ~ list(
      name = .x,
      sym = rlang::sym(.x)
    )
  ) |>
    stats::setNames(names(data_cols))

  if (!base::is.null(block_cols)) {
    block_cols <- list(name = block_cols, symbol = rlang::syms(block_cols))
  }
  character_args <- purrr::map(
    list(
      assignment_method = assignment_method,
      algorithm = algorithm,
      time_unit = time_unit,
      prior_periods = prior_periods
    ),
    ~ {
      if (is.character(.x)) {
        base::tolower(.x)
      } else {
        .x
      }
    }
  )

  # Input Validation
  if (check_args) {
    validate_inputs(
      data = data,
      time_unit = character_args$time_unit,
      perfect_assignment = perfect_assignment,
      algorithm = character_args$algorithm,
      period_length = period_length,
      whole_experiment = whole_experiment,
      prior_periods = character_args$prior_periods,
      data_cols = data_cols,
      block_cols = block_cols,
      blocking = blocking,
      assignment_method = character_args$assignment_method,
      verbose = verbose,
      control_augment = control_augment,
      ndraws = ndraws,
      random_assign_prop = random_assign_prop
    )
  }
  conditions <- create_conditions(
    control_condition = control_condition,
    data = data,
    condition_col = data_cols$condition_col,
    control_augment = control_augment
  )

  # Preparing Data to be simulated
  verbose_log(verbose, "Preparing Data")

  data <- create_cutoff(
    data = data,
    data_cols = data_cols,
    period_length = period_length,
    assignment_method = character_args$assignment_method,
    time_unit = character_args$time_unit
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
    imputation_information = imputation_information,
    character_args = character_args,
    conditions = conditions
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
#' @returns: A named list containing:
#' \itemize{
#' \item `final_data`: The processed tibble or data.table, containing new columns pertaining to the results of the trial.
#' \item `bandits`: A tibble or data.table containing the UCB1 valuess or Thompson sampling posterior distributions for each period.
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

mab_simulation <- function(
  data,
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
  assignment_method,
  control_augment,
  imputation_information,
  ndraws,
  random_assign_prop
) {
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
    imputation_information = imputation_information,
    ndraws = ndraws,
    random_assign_prop
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
    settings = NULL
  )
  return(results)
}
#---------------------------------------------------------------------------------
#' @title Creating proper conditions vector
#' @name create_conditions
#' @returns Character vector of unique treatment conditions. Throws error if an invalid specification
#' is used.
#' @description This function creates a character vector of treatment conditions
#' using the conditions column in the provided data, and if `control_augment` is greater
#' than 0, it also labels the control condition. Throws an error of `control_condition` is not
#' present.
#' @inheritParams single_mab_simulation
#' @inheritParams cols
#' @keywords internal
create_conditions <- function(
  control_condition,
  data,
  condition_col,
  control_augment
) {
  conditions <- base::sort(base::as.character(base::unique(data[[
    condition_col$name
  ]])))
  if (control_augment > 0) {
    if (length(control_condition) != 1) {
      rlang::abort(c(
        "`control_condition` must have a length of 1",
        "x" = sprintf(
          "You passed a vector of length: %d",
          length(control_condition)
        )
      ))
    }
    if (
      is.null(control_condition) |
        is.na(control_condition) |
        !as.character(control_condition) %in% conditions
    ) {
      rlang::abort(c(
        "`control_condition` is not present in the conditions column",
        "x" = sprintf(
          "Potential Conditions: %s",
          paste0(conditions, collapse = ", ")
        ),
        "x" = paste0("You Passed: ", base::deparse(control_condition))
      ))
    }

    names(conditions) <- base::ifelse(
      conditions == as.character(control_condition),
      "control",
      "treatment"
    )
  }
  return(conditions)
}
