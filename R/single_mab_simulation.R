#' @title
#' Running Multi-Arm Bandit Trial and Adaptive Inference
#' @name single_mab_simulation
#'
#' @description Performs a single Multi-Arm Bandit (MAB) trial using experimental data from
#' an original randomized controlled trial, and adaptive inference strategies as described in
#' Hadad et al. (2021). This function wraps around [run_mab_trial()] and [get_adaptive_aipw()],
#' completing the full MAB pipeline: treatment assignment, success imputation, and estimation.
#'
#
#'
#' @param data A data frame that provides the input data for the trial.
#' @param time_unit A string specifying the unit of time for assigning periods.
#'                  Acceptable values are "Day", "Week", or "Month".
#' @param perfect_assignment Logical; if TRUE, assumes perfect information for treatment assignment
#'                           (i.e., all outcomes are observed regardless of the date).
#'                           If FALSE, hides outcomes not yet theoretically observed, based on
#'                           `letter_sent_date` from the `tanf` dataset.
#' @param algorithm A string specifying the MAB algorithm to use. Options are "Thompson" or "UCB1".
#' @param period_length Numeric; length of each treatment period.
#' If assignment method is "Date", this refers to the
#' length of periods by your specified `time_unit` (i.e., if "Day", 10 would be 10 days).
#' If assignment methods is "Batch", this refers to the number of people in each batch.
#' @param prior_periods Numeric; number of previous periods to use in the treatment assignment model
#' or specify string "All" to use all previous periods.
#' @param whole_experiment Logical; if TRUE, uses all past experimental data for imputing outcomes.
#'                         If FALSE, uses only data available up to the current period.
#' @param conditions Named Character vector containing treatment conditions, Control condition, must be named "Control".
#' @param blocking Logical; Whether or not to use treatment blocking.
#' @param block_cols Character Vector of variables to block by.
#' @param date_col Column in data, contains date of treatment.
#' @param month_col Column in data, contains month of treatment.
#' @param id_col Column in data, contains unique id as a key.
#' @param condition_col Column in data, contains original treatment conditions.
#' @param success_col Column in data, contains binary successes.
#' @param success_date_col Column in data, contains date each success occurred.
#' @param assignment_date_col Column in data, contains date of wave assignment.
#' @param verbose Logical; Whether or not to print iteration number. FALSE by default.
#' @param assignment_method String; "Date" or "Batch" to define the assignment into treatment waves.
#' @param control_augment Number \eqn{\in} \[0,1\]; Proportion of each wave guaranteed to get "Control" treatment.
#' Default is 0. Creates potential conflicts with [randomizr::complete_ra()], when not set to 0.
#'
#'
#' @return  A custom mab class object, which is a named list containing:
#' \item{final_data}{The processed data with treatment assignments and imputed outcomes, labelled with "mab_" prefix.}
#' \item{bandits}{Either the UCB1 statistics or Thompson Sampling posterior distributions.}
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
                                  time_unit = NULL,
                                  perfect_assignment,
                                  algorithm,
                                  period_length = NULL,
                                  prior_periods,
                                  whole_experiment,
                                  conditions,
                                  blocking,
                                  block_cols = NULL,
                                  date_col = NULL,
                                  month_col = NULL,
                                  id_col,
                                  condition_col,
                                  success_col,
                                  success_date_col = NULL,
                                  assignment_date_col = NULL,
                                  verbose = FALSE, assignment_method,
                                  control_augment = 0) {
  data_name <- deparse(substitute(data))
  # Input Validation


  check_args(
    data = data, time_unit = time_unit,
    perfect_assignment = perfect_assignment,
    algorithm = algorithm, period_length = period_length,
    whole_experiment = whole_experiment, prior_periods = prior_periods,
    conditions = conditions, blocking = blocking,
    block_cols = block_cols, date_col = {{ date_col }},
    id_col = {{ id_col }}, success_col = {{ success_col }},
    condition_col = {{ condition_col }},
    verbose = verbose,
    success_date_col = {{ success_date_col }},
    assignment_date_col = {{ assignment_date_col }},
    month_col = {{ month_col }},
    assignment_method
  )

  data <- mab_prepare(
    data = data,
    date_col = {{ date_col }},
    time_unit = time_unit,
    period_length = period_length,
    success_col = {{ success_col }},
    condition_col = {{ condition_col }},
    success_date_col = {{ success_date_col }},
    month_col = {{ month_col }},
    perfect_assignment = perfect_assignment,
    assignment_method = assignment_method,
    blocking = blocking,
    block_cols = block_cols,
    verbose = verbose
  )

  verbose_log(verbose, "Precomputing")

  imputation_information <- imputation_prep(
    data = data,
    whole_experiment = whole_experiment,
    success_col = {{ success_col }},
    success_date_col = {{ success_date_col }},
    perfect_assignment = perfect_assignment,
    condition_col = {{ condition_col }}
  )

  results <- mab_simulation(
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
    assignment_method = assignment_method,
    control_augment = control_augment,
    imputation_information = imputation_information
  )
  results$settings$data <- data_name

  return(results)
}
