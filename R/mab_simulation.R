#' @title Simulates Multi-Arm Bandit Trial From Prepared Inputs
#' @name mab_simulation
#'
#' @description This function acts as an internal helper to [single_mab_simulation()]
#' and [multiple_mab_simulation()] by providing a common framework for executing
#' a single Multi-Arm-Bandit Trial with adaptive inference. It assumes all inputs have
#' been preprocessed by [mab_prepare()], or inside of the wrappers. Centralizes
#' a single trial to prevent redundant loops.
#' @inheritParams single_mab_simulation
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
                           period_length = NULL,
                           prior_periods,
                           whole_experiment,
                           conditions,
                           blocking,
                           block_cols = NULL,
                           date_col,
                           month_col = NULL,
                           id_col,
                           condition_col,
                           success_col,
                           success_date_col = NULL,
                           assignment_date_col = NULL,
                           verbose,
                           assignment_method, control_augment) {
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
    date_col = {{ date_col }},
    month_col = {{ month_col }},
    id_col = {{ id_col }},
    condition_col = {{ condition_col }},
    success_col = {{ success_col }},
    success_date_col = {{ success_date_col }},
    assignment_date_col = {{ assignment_date_col }},
    verbose = verbose,
    control_augment = control_augment
  )

  results <- get_adaptive_aipw(
    mab = results,
    conditions = conditions, algorithm = algorithm,
    verbose = verbose
  )

  class(results) <- c("mab", class(results))

  data_name <- base::deparse(base::substitute(data))


  results$settings <- list(
    data = data_name,
    assignment_method = assignment_method,
    time_unit = time_unit,
    perfect_assignment = perfect_assignment,
    algorithm = algorithm,
    period_length = period_length,
    prior_periods = prior_periods,
    whole_experiment = whole_experiment,
    conditions = conditions,
    blocking = blocking,
    block_cols = block_cols
  )

  return(results)
}
