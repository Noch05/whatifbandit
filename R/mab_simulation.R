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
#'
#'
#' @return `mab` class object, which is named list containing:
#' \itemize{
#' \item  `final_data`: The processed data with treatment assignments and imputed outcomes, labelled with "mab_" prefix.
#' \item `bandits`: Either the UCB1 statistics or Thompson Sampling posterior distributions.
#' \item `assignment_probs`: Probability of being assigned each treatment arm at a given period
#' \item `estimates`: AIPW (Augmented Inverse Probability Weighting) treatment effect estimates and variances.
#' \item `settings`: A list of the configuration settings used in the trial.
#' }
#' @seealso
#'* [single_mab_simulation()]
#'* [multiple_mab_simulation()]
#'* [run_mab_trial()]
#'* [get_adaptive_aipw()]
#'* [pre_mab_simulation()]
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

  inference_results <- get_adaptive_aipw(
    data = sim_results$final_data,
    assignment_probs = sim_results$assignment_probs,
    periods = base::max(sim_results$final_data$period_number, na.rm = TRUE),
    conditions = conditions,
    verbose = verbose
  )
  results <- list(
    final_data = inference_results$final_data,
    bandits = sim_results$bandits,
    assignment_probs = sim_results$assignment_probs,
    estimates = inference_results$estimates,
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
