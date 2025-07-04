#' @title Simulates Multi-Arm Bandit Trial From Prepared Inputs
#' @name mab_simulation
#'
#' @description Internal helper to[single_mab_simulation()]
#' and [multiple_mab_simulation()]. Centralizes necessary functions to conduct a
#' single Multi-Arm-Bandit Trial with adaptive inference. It assumes all inputs have
#' been preprocessed by [mab_prepare()].
#' @inheritParams single_mab_simulation
#' @inheritParams run_mab_trial
#'
#'
#'
#'
#' @seealso
#'* [single_mab_simulation()]
#'* [multiple_mab_simulation()]
#'* [run_mab_trial()]
#'* [get_adaptive_aipw()]
#'* [mab_prepare()]
#'
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
  conditions <- base::sort(conditions)

  # Run the main MAB trial with all required arguments
  results <- run_mab_trial(
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

  results <- get_adaptive_aipw(
    mab = results,
    conditions = conditions, algorithm = algorithm,
    verbose = verbose
  )

  class(results) <- c("mab", class(results))


  results$settings <- list(
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

  return(results)
}
