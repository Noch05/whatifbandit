#' Runs Multi-Arm Bandit Trial
#' @name run_mab_trial
#'
#' @description Performs a full Multi-Arm Bandit (MAB) trial using Thompson Sampling or UCB1.
#' The function provides loop around each step of the process for each treatment wave, performing adaptive
#' treatment assignment, and outcome imputation. Supports flexible customization passed
#' from [single_mab_simulation()] and [multiple_mab_simulation()] in treatment blocking strategy,
#' stationary/non-stationary bandits, control augmentation, and hybrid assignment.
#'
#' @inheritParams single_mab_simulation
#' @param imputation_information Object created by [imputation_precompute()] containing the conditional means and success dates
#' for each treatment block to impute from.
#' @inheritParams cols
#'
#'
#' @returns  A named list containing:
#' \itemize{
#' \item `final_data`: The processed tibble or data.table, containing new columns pertaining to the results of the trial.
#' \item `bandits`: A tibble or data.table containing the UCB1 statistics or Thompson Sampling posterior distributions for each period.
#' \item `assignment_probs`: A tibble or data.table containing the probability of being assigned each treatment arm at a given period.
#' }
#' @details
#' The first period is used to start the trial, so the MAB loop
#' starts at period number 2.
#'
#' @keywords internal
#'
run_mab_trial <- function(data, time_unit, period_length = NULL,
                          data_cols, block_cols, blocking,
                          prior_periods, algorithm,
                          whole_experiment, perfect_assignment, conditions,
                          verbose, control_augment,
                          imputation_information, ndraws, random_assign_prop) {
  periods <- base::max(data$period_number)

  bandits <- base::vector(mode = "list", length = 2)
  bandits$bandit_stat <- base::vector(mode = "list", length = (periods + 1))
  bandits$assignment_prob <- base::vector(mode = "list", length = periods)
  num_conditions <- length(conditions)

  bandits$bandit_stat[[1]] <- switch(algorithm,
    "Thompson" = rlang::set_names(rep_len(1 / num_conditions,
      length.out = num_conditions
    ), conditions),
    "UCB1" = tibble::tibble(mab_condition = conditions, ucb = rep_len(0, num_conditions)),
    rlang::abort("Invalid Algorithm: Valid Algorithms are `Thompson` and `UCB`")
  )
  bandits$assignment_prob[[1]] <- rlang::set_names(rep_len(1 / num_conditions,
    length.out = num_conditions
  ), conditions)

  verbose_log(verbose, "Starting Bandit Trial")
  for (i in 2:periods) {
    verbose_log(verbose, paste0("Period: ", i))

    prior <- create_prior(prior_periods = prior_periods, current_period = i)

    if (inherits(data, "data.table")) {
      current_data <- data[period_number == i, ]
      prior_data <- data[period_number %in% prior, ]
    } else {
      current_data <- data[data$period_number == i, ]
      prior_data <- data[data$period_number %in% prior, ]
    }

    past_results <- get_past_results(
      current_data = current_data,
      prior_data = prior_data,
      perfect_assignment = perfect_assignment,
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
      algorithm = algorithm,
      id_col = data_cols$id_col,
      conditions = conditions,
      condition_col = data_cols$condition_col,
      success_col = data_cols$success_col,
      random_assign_prop = random_assign_prop
    )

    bandits$assignment_prob[[i]] <- (bandit[["assignment_prob"]] * (1 - random_assign_prop)) +
      (rep_len(1 / num_conditions, length.out = num_conditions) * random_assign_prop)

    prepped_impute <- imputation_preparation(
      current_data = current_data,
      whole_experiment = whole_experiment,
      imputation_information = imputation_information,
      block_cols = block_cols,
      blocking = blocking,
      perfect_assignment,
      current_period = i
    )

    data <- impute_success(
      current_data = prepped_impute$current_data,
      imputation_info = prepped_impute$impute_success,
      dates = prepped_impute$impute_dates,
      id_col = data_cols$id_col,
      success_col = data_cols$success_col,
      prior_data = data,
      perfect_assignment = perfect_assignment,
      success_date_col = data_cols$success_date_col,
      current_period = i
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
#' \item `final_data`: The processed tibble or data.table, containing new columns pertaining to the results of the trial.
#' \item `bandits`: A tibble or data.table containing the UCB1 statistics or Thompson Sampling posterior distributions for each period.
#' \item `assignment_probs`: A tibble or data.table containing the probability of being assigned each treatment arm at a given period.
#' }
#' @details
#' Takes the bandit lists provided, and condenses them using [dplyr::bind_rows()]
#' into tibbles or data.tables, and then pivots the table
#' to wide format where each treatment arm is a column, and the rows
#' represent periods.
#'
#' At this step the final UCB1 or Thompson Probabilities are calculated,
#' and the whole table is lead by one to have each period be represented
#' by the calculation that occurred just after completing the period, so
#' period 11 now means the bandit calculated in period 12, using the results
#' from period 11. The assignment probabilities are not changed in this way, so for each period
#' they still reflect the assignment probabilities used in that period.
#'
#' @seealso
#'* [run_mab_trial()]
#' @keywords internal

end_mab_trial <- function(data, bandits, algorithm, periods, conditions, ndraws) {
  base::UseMethod("end_mab_trial", data)
}
#-------------------------------------------------------------------------------
#
#' @method end_mab_trial data.frame
#' @inheritParams end_mab_trial
#' @title [end_mab_trial()] for data.frames
#' @noRd
end_mab_trial.data.frame <- function(data, bandits, algorithm, periods, conditions, ndraws) {
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

  bandit_stats <- switch(algorithm,
    "Thompson" = {
      dplyr::bind_rows(bandits$bandit_stat, .id = "period_number") |>
        dplyr::mutate(
          period_number = base::as.numeric(period_number),
          dplyr::across(-period_number, ~ dplyr::lead(., n = 1L, default = NA))
        ) |>
        dplyr::slice(base::seq_len(periods))
    },
    "UCB1" = {
      dplyr::bind_rows(bandits$bandit_stat, .id = "period_number") |>
        dplyr::select(ucb, mab_condition, period_number) |>
        tidyr::pivot_wider(values_from = "ucb", names_from = c("mab_condition")) |>
        dplyr::mutate(
          period_number = base::as.numeric(period_number),
          dplyr::across(-period_number, ~ dplyr::lead(., n = 1L, default = NA))
        ) |>
        dplyr::slice(base::seq_len(periods))
    },
    rlang::abort("Invalid Algorithm: valid algorithsm are `Thompson`, and `UCB1`")
  )


  assignment_probs <- dplyr::bind_rows(bandits$assignment_prob, .id = "period_number") |>
    dplyr::mutate(period_number = base::as.numeric(period_number))

  return(list(
    final_data = data,
    bandits = bandit_stats,
    assignment_probs = assignment_probs
  ))
}
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#' @method end_mab_trial data.table
#' @inheritParams end_mab_trial
#' @title [end_mab_trial()] for data.tables
#' @noRd
end_mab_trial.data.table <- function(data, bandits, algorithm, periods, conditions, ndraws) {
  final_summary <- data[, .(
    successes = base::sum(mab_success, na.rm = TRUE),
    success_rate = base::mean(mab_success, na.rm = TRUE),
    n = .N
  ), by = mab_condition]

  final_bandit <- get_bandit(
    past_results = final_summary,
    algorithm = algorithm,
    conditions = conditions,
    current_period = (periods + 1),
    control_augment = 0,
    ndraws = ndraws
  )

  bandits$bandit_stat[[(periods + 1)]] <- final_bandit[[1]]

  bandit_stats <- switch(algorithm,
    "Thompson" = {
      x <- data.table::as.data.table(dplyr::bind_rows(bandits$bandit_stat,
        .id = "period_number"
      ))
      x[, period_number := base::as.numeric(period_number)]

      x[, (conditions) := lapply(.SD, function(col) {
        data.table::shift(col,
          n = 1L,
          type = "lead",
          fill = NA
        )
      }),
      .SDcols = conditions
      ]
      x[base::seq_len(periods), ]
    },
    "UCB1" = {
      x <- data.table::rbindlist(bandits$bandit_stat,
        use.names = TRUE,
        fill = TRUE,
        idcol = "period_number"
      )
      x <- data.table::dcast(
        data = x[, .(ucb, mab_condition, period_number)],
        formula = period_number ~ mab_condition, value.var = "ucb"
      )

      x[, period_number := base::as.numeric(period_number)]

      x[, (conditions) := base::lapply(.SD, function(col) {
        data.table::shift(col,
          n = 1L,
          type = "lead",
          fill = NA
        )
      }),
      .SDcols = conditions
      ]
      x[base::seq_len(periods), ]
    },
    rlang::abort("Invalid Algorithm: valid algorithsm are `Thompson`, and `UCB1`")
  )

  assignment_probs <- data.table::as.data.table(dplyr::bind_rows(bandits$assignment_prob, .id = "period_number"))
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
#' @description Used during [run_mab_trial()] to create a vector of prior periods dynamically. Used to create
#' the proper vector when `prior_periods` is not "All".
#'
#' @inheritParams single_mab_simulation
#' @param current_period The current period of the simulation. Defined by loop structure inside [run_mab_trial()].
#'
#'
#' @returns Numeric vector containing the prior treatment periods to be used when aggregating
#' the results for the current treatment assignment period.
#'
#' @seealso
#' *[run_mab_trial()]
#' @keywords internal

create_prior <- function(prior_periods, current_period) {
  if (prior_periods == "All" || prior_periods >= current_period) {
    ## Looking at all the past periods

    prior <- base::seq_len(current_period - 1)
  } else if (prior_periods < current_period) {
    # returns x most recent periods, i.e. if prior is 3, and current is 6, returns 3:5

    prior <- base::seq(from = current_period - prior_periods, to = (current_period - 1), by = 1)
  } else {
    stop("Invalid Prior Cutoff, specify either a whole number or \"All\"")
  }
  return(prior)
}
