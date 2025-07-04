#' @title
#' Running Multi-Arm Bandit Trial and Adaptive Inference
#' @name single_mab_simulation
#'
#' @description Performs a single Multi-Arm Bandit (MAB) trial using experimental data from
#' an original randomized controlled trial, and adaptive inference strategies as described in
#' Hadad et al. (2021). This function wraps around [run_mab_trial()] and [get_adaptive_aipw()],
#' completing the full MAB pipeline: treatment assignment, success imputation, and estimation.
#'
#'
#' @param data A data frame, tibble or data.table that provides the input data for the trial.
#' @param time_unit A string specifying the unit of time for assigning periods when 'assignment_method` is 'date'.
#'  Acceptable values are "Day", "Week", or "Month".
#' @param perfect_assignment Logical; if TRUE, assumes perfect information for treatment assignment
#'                           (i.e., all outcomes are observed regardless of the date).
#'                           If FALSE, hides outcomes not yet theoretically observed, based
#'                           on the dates treatments would have been assigned for each wave.
#' @param algorithm A string specifying the MAB algorithm to use. Options are "Thompson" or "UCB1".
#' @param period_length Numeric; length of each treatment period.
#' If assignment method is "Date", this refers to the
#' length of periods by your specified `time_unit` (i.e., if "Day", 10 would be 10 days).
#' If assignment methods is "Batch", this refers to the number of people in each batch.
#' @param prior_periods Numeric; number of previous periods to use in the treatment assignment model
#' or specify string "All" to use all previous periods.
#' @param whole_experiment Logical; if TRUE, uses all past experimental data for imputing outcomes.
#'                         If FALSE, uses only data available up to the current period.
#' @param conditions Named Character vector containing treatment conditions.
#' Control condition, must be named "Control" when 'control_augment' > 0.
#' @param data_cols Named Character vector containing the names of columns in data as strings:
#' \itemize{
#' \item id_col: Column in data, contains unique id as a key.
#' \item success_col: Column in data; Binary successes from original experiment.
#' \item date_col: Column in data, contains original date of event/trial; only ncessary when assigning by 'Date'.
#' \item month_col: Column in data, contains month of treatment; only necessary when time_unit = 'Month'.
#' \item success_date_col: Column in data, contains original dates each success occured; only necessary when 'perfect_assignment' = FALSE.
#' \item assignment_date_col: Column in data, contains original dates treatments are assigned to observations; only necessary when 'perfect_assignment' = FALSE.
#' Used to simulate imperfect information on part of researchers conducting an adaptive trial.
#' }
#' @param blocking Logical; Whether or not to use treatment blocking.
#' @param block_cols Character Vector of variables to block by.
#' @param assignment_method String; "Date" or "Batch" to define the assignment into treatment waves.
#' @param control_augment Number \eqn{\in} \[0,1\]; Proportion of each wave guaranteed to get "Control" treatment.
#' Default is 0.
#' @param verbose Logical; Whether or not to print iteration number. FALSE by default.
#'
#' @return `mab` class object, which is named list containing:
#' \item{final_data}{The processed data with treatment assignments and imputed outcomes, labelled with "mab_" prefix.}
#' \item{bandits}{Either the UCB1 statistics or Thompson Sampling posterior distributions.}
#' \item{assignment_probs}{Probability of being assigned each treatment arm at a given period}
#' \item{estimates}{AIPW (Augmented Inverse Probability Weighting) treatment effect estimates and variances.}
#' \item{settings}{A list of the configuration settings used in the trial.}
#'
#' @seealso
#' * [run_mab_trial()]
#' * [get_adaptive_aipw()]
#' * [check_args()]
#' * [mab_simulation()]
#' * [mab_prepare()]
#' @export

single_mab_simulation <- function(data,
                                  assignment_method,
                                  algorithm,
                                  conditions,
                                  prior_periods,
                                  perfect_assignment,
                                  whole_experiment,
                                  blocking,
                                  data_cols,
                                  control_augment = 0,
                                  time_unit = NULL,
                                  period_length = NULL,
                                  block_cols = NULL,
                                  verbose = FALSE) {
  data_name <- deparse(substitute(data))

  prepped <- pre_mab_simulation(
    data = data, assignment_method = assignment_method,
    algorithm = algorithm, conditions = conditions,
    prior_periods = prior_periods, perfect_assignment = perfect_assignment,
    whole_experiment = whole_experiment, blocking = blocking,
    block_cols = block_cols, data_cols = data_cols,
    control_augment = control_augment, time_unit = time_unit,
    period_length = period_length,
    verbose = verbose
  )


  # Simulating the MAB Trial
  results <- mab_simulation(
    data = prepped$data,
    time_unit = time_unit,
    period_length = period_length,
    prior_periods = prior_periods,
    algorithm = algorithm,
    whole_experiment = whole_experiment,
    perfect_assignment = perfect_assignment,
    conditions = conditions,
    blocking = blocking,
    block_cols = prepped$block_cols,
    data_cols = prepped$data_cols,
    verbose = verbose,
    assignment_method = assignment_method,
    control_augment = control_augment,
    imputation_information = prepped$imputation_information
  )
  results$settings$data <- data_name

  return(results)
}
