#------------------------------------------------------------------------------
#' @title Simulates MAB Trial From Prepared Inputs and Performs Inference
#' @name simulate_mab_rct.bernoulli
#'
#' @description Internal helper. Centralizes necessary functions to conduct a
#' a MAB trial with adaptive inference. It assumes all inputs have been preprocessed already
#' @inheritParams mab_from_rct.bernoulli
#' @inheritParams prepare_rct
#' @param starts  Numeric vector where element `i` is the starting row number of period `i`.
#' @param ends  Numeric vector where element `i` is the ending row number of period `i`.
#' @param imputation_information Object created by [imputation_precompute()] containing the conditional means and success dates
#' for each treatment block to impute from.
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

simulate_mab_rct.bernoulli <- function(
  data,
  algorithm,
  control_augment,
  random_assign_prop,
  period_length,
  prior_periods,
  discount_rate,
  delayed_feedback,
  whole_experiment,
  conditions,
  blocking,
  clustering,
  data_cols,
  imputation_information,
  verbose,
  ndraws,
  starts,
  ends
) {
  periods <- base::length(starts)
  num_conditions <- length(conditions)
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
    "static" = base::rep(1 / num_conditions, num_conditions),
  )
  bandits$assignment_prob[1, ] <- base::rep(1 / num_conditions, num_conditions)

  verbose_log(verbose, "Starting Bandit Trial")

  sim_results <- run_mab_trial(
    data = data,
    algorithm = algorithm,
    control_augment = control_augment,
    random_assign_prop,
    period_length = period_length,
    prior_periods = prior_periods,
    discount_rate = discount_rate,
    whole_experiment = whole_experiment,
    delayed_feedback = delayed_feedback,
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
    bandits = bandits
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
    estimates = estimates,
    settings = NULL
  )
  return(results)
}

#' Runs Multi-Arm Bandit Trial
#' @name run_mab_trial
#'
#' @description Performs a full Multi-Arm Bandit (MAB) trial using Thompson sampling or UCB1.
#' The function provides loop around each step of the process for each treatment wave, performing adaptive
#' treatment assignment, and outcome imputation. Supports flexible customizations in treatment blocking strategy,
#' stationary/non-stationary bandits, control augmentation, and hybrid assignment.
#'
#' @inheritParams simulate_mab_rct.bernoulli
#' @inheritParams mab_from_rct.bernoulli
#' @inheritParams prepare_rct
#'
#'
#' @returns  A named list containing:
#' \itemize{
#' \item `final_data`: The processed `tibble` or `data.table`, containing new columns pertaining to the results of the trial.
#' \item `bandits`: A `tibble` or `data.table` containing the UCB1 values or Thompson sampling posterior distributions for each period.
#' \item `assignment_probs`: A `tibble` or `data.table` containing the probability of being assigned each treatment arm at a given period.
#' }
#' @details
#' The first period is used to start the trial, so the MAB loop
#' starts at period number 2.
#'
#' @keywords internal
#'
run_mab_trial <- function(
  data,
  period_length = NULL,
  data_cols,
  clustering,
  blocking,
  prior_periods,
  algorithm,
  whole_experiment,
  delayed_feedback,
  conditions,
  verbose,
  control_augment,
  imputation_information,
  ndraws,
  random_assign_prop,
  starts,
  ends,
  periods,
  impute_cluster,
  rct,
  bandits
) {
  for (i in 2:periods) {
    verbose_log(verbose, paste0("Period: ", i))

    prior <- create_prior(prior_periods = prior_periods, current_period = i)

    current_data <- data[starts[i]:ends[i], ]
    prior_data <- data[starts[prior]:ends[i - 1], ]

    past_results <- get_past_results(
      current_data = current_data,
      prior_data = prior_data,
      delayed_feedback = delayed_feedback,
      assignment_date_col = data_cols$assignment_date_col,
      conditions = conditions
    )

    bandit <- get_bandit(
      past_results = past_results,
      algorithm = algorithm,
      conditions = conditions,
      current_period = i,
      control_augment = control_augment,
      ndraws = ndraws
    )

    bandits$bandit_stat[[i]] <- bandit[["bandit"]]

    current_data <- assign_treatments(
      current_data = current_data,
      probs = bandit[["assignment_prob"]],
      blocking = blocking,
      clustering = clustering,
      cluster_col,
      conditions = conditions,
      condition_col = data_cols$condition_col,
      random_assign_prop = random_assign_prop
    )

    bandits$assignment_prob[[i]] <- (bandit[["assignment_prob"]] *
      (1 - random_assign_prop)) +
      (rep(1 / num_conditions, num_conditions) *
        random_assign_prop)

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

    data <- impute_success(
      current_data = prepped_impute$current_data,
      imputation_info = prepped_impute$impute_success,
      dates = prepped_impute$impute_dates,
      id_col = data_cols$id_col,
      success_col = data_cols$success_col,
      prior_data = data,
      delayed_feedback = delayed_feedback,
      success_date_col = data_cols$success_date_col,
      current_period = i,
      starts = starts,
      ends <- ends
    )
  }
  results <- end_mab_trial(
    data = data,
    bandits = bandits,
    algorithm = algorithm,
    conditions = conditions,
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
#' @param bandits Finalized bandits list from [run_mab_trial()].
#' @param periods Numeric value of length 1; total number of periods in Multi-Arm-Bandit trial.
#' @inheritParams single_mab_simulation
#' @returns  A named list containing:
#' \itemize{
#' \item `final_data`: The processed `tibble` or `data.table`, containing new columns pertaining to the results of the trial.
#' \item `bandits`: A `tibble` or `data.table` containing the UCB1 values or Thompson sampling posterior distributions for each period.
#' \item `assignment_probs`: A `tibble` or `data.table` containing the probability of being assigned each treatment arm at a given period.
#' }
#' @details
#' Takes the bandit lists provided, and condenses them using [dplyr::bind_rows()]
#' into `tibble`s or `data.table`s, and then pivots the table
#' to wide format where each treatment arm is a column, and the rows
#' represent periods.
#'
#' At this step the final UCB1 or Thompson sampling probabilities are calculated.
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
    conditions = conditions,
    current_period = (periods + 1),
    control_augment = 0,
    ndraws = ndraws
  )

  bandits$bandit_stat[[(periods + 1)]] <- final_bandit[[1]]

  bandit_stats <- switch(
    algorithm,
    "thompson" = {
      dplyr::bind_rows(bandits$bandit_stat, .id = "period_number") |>
        dplyr::mutate(
          period_number = base::as.numeric(period_number),
          dplyr::across(-period_number, ~ dplyr::lead(., n = 1L, default = NA))
        ) |>
        dplyr::slice(base::seq_len(periods))
    },
    "ucb1" = {
      dplyr::bind_rows(bandits$bandit_stat, .id = "period_number") |>
        dplyr::select(ucb, mab_condition, period_number) |>
        tidyr::pivot_wider(
          values_from = "ucb",
          names_from = c("mab_condition")
        ) |>
        dplyr::mutate(
          period_number = base::as.numeric(period_number),
          dplyr::across(-period_number, ~ dplyr::lead(., n = 1L, default = NA))
        ) |>
        dplyr::slice(base::seq_len(periods))
    },
    rlang::abort(
      "Invalid Algorithm: valid algorithms are `thompson`, and `ucb1`"
    )
  )

  assignment_probs <- dplyr::bind_rows(
    bandits$assignment_prob,
    .id = "period_number"
  ) |>
    dplyr::mutate(period_number = base::as.numeric(period_number))

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
    conditions = conditions,
    current_period = (periods + 1),
    control_augment = 0,
    ndraws = ndraws
  )
  conditions <- as.character(conditions) # Converting to character for reference in Data.table Syntax

  bandits$bandit_stat[[(periods + 1)]] <- final_bandit[[1]]

  bandit_stats <- switch(
    algorithm,
    "thompson" = {
      x <- base::lapply(seq_len(periods + 1), function(i) {
        base::as.list(bandits$bandit_stat[[i]])
      }) |>
        data.table::rbindlist(idcol = "period_number", use.names = TRUE)
      x[, period_number := base::as.numeric(period_number)]

      x[,
        (conditions) := lapply(.SD, function(col) {
          data.table::shift(col, n = 1L, type = "lead", fill = NA)
        }),
        .SDcols = conditions
      ]
      x[base::seq_len(periods), ]
    },
    "ucb1" = {
      x <- data.table::rbindlist(
        bandits$bandit_stat,
        use.names = TRUE,
        fill = TRUE,
        idcol = "period_number"
      )
      x <- data.table::dcast(
        data = x[, .(ucb, mab_condition, period_number)],
        formula = period_number ~ mab_condition,
        value.var = "ucb"
      )

      x[, period_number := base::as.numeric(period_number)]

      x[,
        (conditions) := base::lapply(.SD, function(col) {
          data.table::shift(col, n = 1L, type = "lead", fill = NA)
        }),
        .SDcols = conditions
      ]
      x[base::seq_len(periods), ]
    },
    rlang::abort(
      "Invalid Algorithm: valid algorithsm are `thompson`, and `ucb1`"
    )
  )

  assignment_probs <- base::lapply(seq_len(periods), function(i) {
    base::as.list(bandits$assignment_prob[[i]])
  }) |>
    data.table::rbindlist(idcol = "period_number", use.names = TRUE)
  assignment_probs[, period_number := base::as.numeric(period_number)]

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
#' @inheritParams mab_from_rct.bernoulli()
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
