#' Runs Multi-Arm Bandit Trial
#' @name run_mab_trial
#'
#' @description Performs a full Multi-Arm Bandit (MAB) trial using Thompson Sampling or UCB1.
#' The function provides loop around each step of the process for each treatment wave, performing adaptive
#' treatment assignment, and outcome imputation. Supports flexible customization in treatment blocking strategy,
#' the size of each treatment wave, and information availability to simulate both a real experiment, and non-stationary
#' bandit strategy.
#'
#' @inheritParams single_mab_simulation
#' @param imputation_information Object created by [imputation_prep()] containing the conditional means and success dates
#' for each treatment block to impute from.
#' @inheritParams cols
#'
#'
#' @return  A named list containing:
#' \itemize{
#' \item `final_data`: Processed data with new treatment assignments and imputed outcomes labelled with "mab_" prefix.
#' \item `bandits`: Thompson Probability or UCB1 statistic for each treatment arm at each period of the simulation.
#' \item `assignment_probs`: Assignment probabilities for each treatment arm at each period of the simulation.}
#'
#' @seealso
#' * [single_mab_simulation()]
#' * [mab_simulation()]
#' * [create_prior()]
#' * [get_bandit()]
#' * [assign_treatments()]
#' * [check_impute()]
#' * [get_past_results()]
#' * [impute_success()]
#'
#'
run_mab_trial <- function(data, time_unit, period_length = NULL,
                          data_cols, block_cols, blocking,
                          prior_periods, algorithm,
                          whole_experiment, perfect_assignment, conditions,
                          verbose, control_augment,
                          imputation_information) {
  periods <- base::max(data$period_number)

  bandits <- base::vector(mode = "list", length = 2)
  bandits$bandit_stat <- base::vector(mode = "list", length = (periods + 1))
  bandits$assignment_prob <- base::vector(mode = "list", length = periods)

  bandits$bandit_stat[[1]] <- switch(algorithm,
    "Thompson" = rlang::set_names(rep(1 / length(conditions), length(conditions)), conditions),
    "UCB1" = tibble::tibble(mab_condition = conditions, ucb = rep(0, length(conditions))),
    rlang::abort("Invalid Algorithm: Valid Algorithms are `Thompson` and `UCB`")
  )
  bandits$assignment_prob[[1]] <- rlang::set_names(rep(1 / length(conditions), length(conditions)), conditions)

  verbose_log(verbose, "Starting Bandit Trial")
  for (i in 2:periods) {
    verbose_log(verbose, paste0("Period: ", i))

    prior <- create_prior(prior_periods = prior_periods, current_period = i)

    if (!inherits(data, "data.table")) {
      current_data <- data[data$period_number == i, ]
      prior_data <- data[data$period_number %in% prior, ]
    } else {
      current_data <- data
      prior_data <- NULL
    }

    past_results <- get_past_results(
      current_data = current_data,
      prior_data = prior_data,
      current_period = i,
      prior = prior,
      perfect_assignment = perfect_assignment,
      assignment_date_col = data_cols$assignment_date_col,
      conditions = conditions
    )

    bandit <- get_bandit(
      past_results = past_results,
      algorithm = algorithm,
      conditions = conditions,
      current_period = i,
      control_augment = control_augment
    )

    bandits$assignment_prob[[i]] <- bandit[[2]]
    bandits$bandit_stat[[i]] <- bandit[[1]]

    current_data <- assign_treatments(
      current_data = current_data,
      probs = bandit[[2]],
      blocking = blocking,
      algorithm = algorithm,
      id_col = data_cols$id_col,
      conditions = conditions,
      condition_col = data_cols$condition_col,
      success_col = data_cols$success_col,
      current_period = i
    )
    prepped_impute <- impute_loop_prep(
      current_data = current_data,
      current_period = i,
      whole_experiment = whole_experiment,
      imputation_information = imputation_information,
      block_cols = block_cols,
      blocking = blocking,
      perfect_assignment
    )

    data <- impute_success(
      current_data = prepped_impute$current_data,
      imputation_info = prepped_impute$impute_success,
      dates = prepped_impute$impute_dates,
      id_col = data_cols$id_col,
      success_col = data_cols$success_col,
      prior_data = data,
      perfect_assignment = perfect_assignment,
      success_date_col = data_cols$success_date_col
    )
  }

  results <- end_mab_trial(
    data = data, bandits = bandits, algorithm = algorithm,
    conditions = conditions, periods = periods
  )



  return(results)
}
