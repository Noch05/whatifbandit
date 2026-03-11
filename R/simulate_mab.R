#------------------------------------------------------------------------------
#' @title Simulates MAB Trial From Prepared Inputs and Performs Inference
#' @name simulate_mab
#' @description Internal helper. Centralizes necessary functions to conduct a
#' a MAB trial with adaptive inference. It assumes all inputs have been preprocessed already
#' @inheritParams mab_from_rct.bernoulli
#' @inheritParams prepare_rct
#' @param starts  Numeric vector where element `i` is the starting row number of period `i`.
#' @param ends  Numeric vector where element `i` is the ending row number of period `i`.
#' @param imputation_information Object created by [imputation_precompute()] containing the conditional means and success dates
#' for each treatment block to impute from.
#' @param resimulation Logical flag; Whether or not this MAB Trial is being run as a re-simulated RCT, as opposed to an original simulation from specified
#' population parameters.
#' @param true_prob True probabilities of success, used to generate outcomes in the case of an original simulation.
#'
#' @inheritParams mab_trial_sim.bernoulli
#'
#'
#' @returns: A named list containing:
#' \itemize{
#' \item `final_data`: The processed `tibble` or `data.table`, containing new columns pertaining to the results of the trial.
#' \item `bandits`: A `tibble` or `data.table` containing the UCB1 values or Thompson Sampling posterior distributions for each period.
#' \item `assignment_probs`: A `tibble` or `data.table` containing the probability of being assigned each treatment arm at a given period.
#' \item `estimates`: A `tibble` or `data.table` containing all estimates of the means and variances related to the treatment arms.
#' \item `settings`: A named list of the configuration settings used in the trial.
#' }
#' @keywords internal
#'

simulate_mab <- function(
  data,
  resimulation,
  true_prob,
  algorithm,
  control_augment,
  random_assign_prop,
  period_length,
  prior_periods,
  discount_rate,
  delayed_feedback,
  whole_experiment = NULL,
  conditions,
  blocking,
  clustering,
  data_cols,
  imputation_information = NULL,
  verbose,
  ndraws,
  starts,
  ends,
  ...
) {
  verbose_log(verbose, "Starting Bandit Trial")
  periods <- base::length(starts)
  num_conditions <- length(conditions)

  sim_results <- run_mab_trial(
    data = data,
    resimulation = resimulation,
    algorithm = algorithm,
    control_augment = control_augment,
    random_assign_prop,
    period_length = period_length,
    prior_periods = prior_periods,
    discount_rate = discount_rate,
    whole_experiment = whole_experiment,
    delayed_feedback = delayed_feedback,
    num_conditions = num_conditions,
    conditions = conditions,
    blocking = blocking,
    clustering = clustering,
    data_cols = data_cols,
    imputation_information = imputation_information,
    verbose = verbose,
    ndraws = ndraws,
    starts = starts,
    ends = ends,
    periods = periods,
    true_prob = true_prob,
    ...
  )

  sim_results$final_data <- get_iaipw(
    data = sim_results$final_data,
    assignment_probs = sim_results$assignment_probs,
    conditions = conditions,
    periods = periods,
    verbose = verbose
  )
  estimates <- estimate_aipw(
    data = sim_results$final_data,
    assignment_probs = sim_results$assignment_probss,
    periods = periods,
    conditions = conditions,
    verbose = verbose,
    clustering = clustering,
    cluster_col = data_cols$cluster_col
  )
  estimates <- estimate_ipw(
    data = sim_results$final_data,
    estimates = estimates,
    cluster_col = data_cols$cluster_col,
    clustering = clustering,
    blocking = blocking,
    conditions = conditions
  )

  results <- list(
    final_data = sim_results$final_data,
    bandits = sim_results$bandits,
    assignment_probs = sim_results$assignment_probs,
    estimates = estimates$est,
    ipw_vcov = estimates$vcov,
    settings = NULL
  )
  return(results)
}

#' Runs Multi-Arm Bandit Trial
#' @name run_mab_trial
#'
#' @description Performs a full Multi-Arm Bandit (MAB) trial using Thompson Sampling or UCB1.
#' The function provides loop around each step of the process for each treatment wave, performing adaptive
#' treatment assignment, and outcome imputation. Supports flexible customizations in treatment blocking strategy,
#' stationary/non-stationary bandits, control augmentation, and hybrid assignment.
#'
#' @inheritParams simulate_mab_rct.bernoulli
#' @inheritParams mab_from_rct.bernoulli
#' @inheritParams prepare_rct
#' @inheritParams mab_trial_sim.bernoulli
#' @param num_conditions Number of conditions, equivalent to `length(conditions)`.
#'
#'
#' @returns  A named list containing:
#' \itemize{
#' \item `final_data`: The processed `tibble` or `data.table`, containing new columns pertaining to the results of the trial.
#' \item `bandits`: A `tibble` or `data.table` containing the UCB1 values or Thompson Sampling posterior distributions for each period.
#' \item `assignment_probs`: A `tibble` or `data.table` containing the probability of being assigned each treatment arm at a given period.
#' }
#' @details
#' The first period is used to initialize the trial, so the MAB loop
#' starts at period number 2.
#'
#' @keywords internal
#'
run_mab_trial <- function(
  data,
  resimulation,
  true_prob,
  algorithm,
  control_augment,
  random_assign_prop,
  period_length = NULL,
  prior_periods,
  discount_rate,
  whole_experiment = NULL,
  delayed_feedback,
  clustering,
  blocking,
  conditions,
  data_cols,
  imputation_information = NULL,
  ndraws,
  verbose,
  starts,
  ends,
  periods,
  num_conditions,
  ...
) {
  bandits <- base::vector(mode = "list", length = 2)
  bandits$bandit_stat <- base::matrix(
    NA,
    nrow = periods + 1,
    ncol = num_conditions,
    dimnames = list(c(), base::names(conditions))
  )
  bandits$assignment_prob <- base::matrix(
    NA,
    nrow = periods,
    ncol = num_conditions,
    dimnames = list(c(), base::names(conditions))
  )

  bandits$bandit_stat[1, ] <- switch(
    algorithm,
    "thompson" = base::rep(1 / num_conditions, num_conditions),
    "ucb1" = base::rep(0, num_conditions),
    "static" = NA,
  )
  bandits$assignment_prob[1, ] <- base::rep(1 / num_conditions, num_conditions)

  equal_probs <- bandits$assignment_prob[1, ] |>
    base::as.numeric() |>
    stats::setNames(conditions)

  for (i in 2:periods) {
    verbose_log(verbose, paste0("Period: ", i))

    prior <- create_prior(prior_periods = prior_periods, current_period = i)

    current_data <- data[starts[i]:ends[i], ]
    prior_data <- data[starts[prior]:ends[i - 1], ]

    if (algorithm != "static") {
      current_bandit <- get_past_results(
        current_data = current_data,
        prior_data = prior_data,
        delayed_feedback = delayed_feedback,
        assignment_date_col = data_cols$assignment_date_col,
        conditions = conditions,
        discount_rate = discount_rate,
        current_period = i
      ) |>
        get_bandit(
          algorithm = algorithm,
          num_conditions = num_conditions,
          conditions = conditions,
          current_period = i,
          control_augment = control_augment,
          ndraws = ndraws
        )
      bandits$bandit_stat[i, ] <- current_bandit[["bandit"]]
    } else {
      current_bandit[["assignment_prob"]] <- equal_probs
    }

    current_data <- assign_treatments(
      current_data = current_data,
      probs = current_bandit[["assignment_prob"]],
      blocking = blocking,
      clustering = clustering,
      cluster_col = data_cols$cluster_col,
      conditions = conditions,
      condition_col = data_cols$condition_col,
      random_assign_prop = random_assign_prop
    )

    bandits$assignment_probs[i, ] <- (current_bandit[["assignment_prob"]] *
      (1 - random_assign_prop)) +
      (equal_probs * random_assign_prop)

    if (resimulation) {
      prepped_impute <- imputation_preparation(
        current_data = current_data,
        whole_experiment = whole_experiment,
        imputation_information = imputation_information,
        data_cols = data_cols,
        clustering = clustering,
        impute_cluster = impute_cluster,
        blocking = blocking,
        delayed_feedback,
        current_period = i
      )

      data[starts[i]:ends[i], ] <- impute_success(
        imputation_info = prepped$impute,
        data_cols = data_cols,
        delayed_feedback = delayed_feedback
      )
    } else {}
  }
  results <- end_mab_trial(
    data = data,
    bandits = bandits,
    algorithm = algorithm,
    conditions = conditions,
    num_conditions = num_conditions,
    periods = periods,
    ndraws = ndraws
  )

  return(results)
}
#-------------------------------------------------------------------------------

#' @name end_mab_trial
#' @title Ends Multi-Arm Bandit Trial
#' @description Condenses output from [run_mab_trial()] into
#' manageable structure.
#' @param data Finalized data from [run_mab_trial()].
#' @param bandits Finalized bandits list of matrices from [run_mab_trial()].
#' @param periods Numeric value of length 1; total number of periods in Multi-Arm-Bandit trial.
#' @inheritParams run_mab_trial
#' @inheritParams simulate_mab
#' @returns  A named list containing:
#' \itemize{
#' \item `final_data`: The processed `tibble` or `data.table`, containing new columns pertaining to the results of the trial.
#' \item `bandits`: A `tibble` or `data.table` containing the UCB1 values or Thompson sampling posterior distributions for each period.
#' \item `assignment_probs`: A `tibble` or `data.table` containing the probability of being assigned each treatment arm at a given period.
#' }
#' @details
#'
#' At this step the final UCB1 or Thompson sampling probabilities are calculated, without discounting.
#' The entire table is shifted backward by one period so that each row reflects the calculation
#' that occurs after completing a period. For example prior to this change, row 11, would indicate the calculations
#' from period 11 before assignment, but now that occured after period 11's imputations.
#'
#' This has the impact of removing the original first row, where all the assignment
#' probabilities are equal, and modifying the last row to represent the final calculation after the conclusion
#' of the simulation.
#'
#' The assignment probabilities are not changed in this way, so for each period
#' they still reflect the assignment probabilities used in that period.
#'
#' @seealso
#' * [run_mab_trial()]
#' @keywords internal

end_mab_trial <- function(
  data,
  bandits,
  algorithm,
  periods,
  conditions,
  num_conditions,
  ndraws
) {
  base::UseMethod("end_mab_trial", data)
}
#-------------------------------------------------------------------------------
#
#' @method end_mab_trial `data.frame`
#' @inheritParams end_mab_trial
#' @title [end_mab_trial()] for `data.frame`s
#' @noRd
end_mab_trial.data.frame <- function(
  data,
  bandits,
  algorithm,
  periods,
  conditions,
  num_conditions,
  ndraws
) {
  final_summary <- data |>
    dplyr::group_by(mab_condition) |>
    dplyr::summarize(
      successes = base::sum(mab_success, na.rm = TRUE),
      success_rate = base::mean(mab_success, na.rm = TRUE),
      n = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::ungroup()

  final_bandit <- get_bandit(
    past_results = final_summary,
    algorithm = algorithm,
    num_conditions = num_conditions,
    conditions = conditions,
    current_period = (periods + 1),
    control_augment = 0,
    ndraws = ndraws
  )

  bandits$bandit_stat[(periods + 1), ] <- final_bandit[["bandit"]]
  assignment_probs <- tibble::as_tibble(bandits[["assignment_prob"]]) |>
    dplyr::mutate(period_number = dplyr::row_number())

  bandit_stats <- tibble::as_tibble(bandits[["bandit_stat"]]) |>
    dplyr::mutate(dplyr::across(tidyselect::everything(), \(x) {
      dplyr::lead(x, n = 1L, default = NA)
    })) |>
    dplyr::slice(base::seq_len(periods)) |>
    dplyr::mutate(period_number == dplyr::row_number())

  return(list(
    final_data = data,
    bandits = bandit_stats,
    assignment_probs = assignment_probs
  ))
}
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#' @method end_mab_trial `data.table`
#' @inheritParams end_mab_trial
#' @title [end_mab_trial()] for `data.table`s
#' @noRd
end_mab_trial.data.table <- function(
  data,
  bandits,
  algorithm,
  periods,
  conditions,
  num_conditions,
  ndraws
) {
  final_summary <- data[,
    .(
      successes = base::sum(mab_success, na.rm = TRUE),
      success_rate = base::mean(mab_success, na.rm = TRUE),
      n = .N
    ),
    by = mab_condition
  ]
  data.table::setorder(final_summary, mab_condition)

  final_bandit <- get_bandit(
    past_results = final_summary,
    algorithm = algorithm,
    num_conditions = num_conditions,
    conditions = conditions,
    current_period = (periods + 1),
    control_augment = 0,
    ndraws = ndraws
  )
  conditions <- as.character(conditions) # Converting to character for reference in Data.table Syntax

  bandits$bandit_stat[periods + 1, ] <- final_bandit[["bandit"]]

  assignment_probs <- data.table::as.data.table(bandits[["assignment_prob"]])
  assignment_probs[, period_number := .I]

  bandit_stats <- data.table::as.data.table(bandits[["bandit_stat"]])
  bandit_stats[,
    (conditions) := base::lapply(.SD, function(col) {
      data.table::shift(col, n = 1L, type = "lead", fill = NA)
    }),
    .SDcols = conditions
  ][base::seq_len(periods), ][, period_number = .I]

  return(list(
    final_data = data,
    bandits = bandit_stats,
    assignment_probs = assignment_probs
  ))
}
#------------------------------------------------------------------------------
#' Create Prior Periods
#' @name create_prior
#' @description Used during [run_mab_trial()] to create a vector of prior periods dynamically based on the specified
#' number of prior periods.
#' @inheritParams mab_from_rct.bernoulli
#' @param current_period The current period of the simulation. Defined by loop structure inside [run_mab_trial()].
#' @returns Numeric value referring to the period index to look back from.
#' the results for the current treatment assignment period.
#'
#' @seealso
#' * [run_mab_trial()]
#' @keywords internal

create_prior <- function(prior_periods = NULL, current_period) {
  if (!base::is.null(prior_periods)) {
    1
  } else {
    current_period - prior_periods
  }
}
