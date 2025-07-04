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
#' \item{final_data}{The processed data with treatment assignments and imputed outcomes labelled with "mab_" prefix.}
#' \item{bandits}{Either the UCB1 statistics or Thompson Sampling posterior distributions.}
#'
#' @seealso
#' * [single_mab_simulation()]
#' * [create_cutoff()]
#' *[create_prior()]
#' *[mab_sample()]
#' *[assign_treatments()]
#' @export
#'
run_mab_trial <- function(data, time_unit, period_length = NULL,
                          data_cols, block_cols, blocking,
                          prior_periods, algorithm,
                          whole_experiment, perfect_assignment, conditions,
                          verbose, control_augment,
                          imputation_information) {
  periods <- base::max(data$period_number)

  bandits <- vector(mode = "list", length = 2)
  bandits[["bandit_stat"]] <- vector(mode = "list", length = (periods + 1))
  bandits[["assignment_prob"]] <- vector(mode = "list", length = periods)

  bandits[["bandit_stat"]][[1]] <- switch(algorithm,
    "Thompson" = rlang::set_names(rep(1 / length(conditions), length(conditions)), conditions),
    "UCB1" = tibble::tibble(mab_condition = conditions, ucb = rep(0, length(conditions))),
    rlang::abort("Invalid Algorithm: Valid Algorithms are `Thompson` and `UCB`")
  )
  bandits[["assignment_prob"]][[1]] <- rlang::set_names(rep(1 / length(conditions), length(conditions)), conditions)

  verbose_log(verbose, "Starting Bandit Trial")
  for (i in 2:periods) {
    verbose_log(verbose, paste0("Period: ", i))

    prior <- create_prior(prior_periods = prior_periods, current_period = i)

    current_data <- data[data$period_number == i, ]
    prior_data <- data[data$period_number < i, ]

    past_results <- get_past_results(
      current_data = current_data,
      prior_data = prior_data,
      perfect_assignment = perfect_assignment,
      success_date_col = data_cols$success_date_col,
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

    bandits[["assignment_prob"]][[i]] <- bandit[[2]]
    bandits[["bandit_stat"]][[i]] <- bandit[[1]]

    current_data <- assign_treatments(
      current_data = current_data,
      probs = bandit[[2]],
      blocking = blocking,
      algorithm = algorithm,
      id_col = data_cols$id_col,
      conditions = conditions,
      condition_col = data_cols$condition_col,
      success_col = data_cols$success_col
    )

    # Creating block for imputing
    if (blocking) {
      current_data[["impute_block"]] <- do.call(
        paste, c(current_data[, c("mab_condition", block_cols$name)], sep = "_")
      )
    } else {
      current_data[["impute_block"]] <- current_data$mab_condition
    }


    if (whole_experiment) {
      impute_info <- imputation_information[[1]]
    } else {
      impute_info <- imputation_information[[1]][[i]]
    }
    if (!perfect_assignment) {
      dates <- rlang::set_names(
        base::as.Date(base::as.numeric(imputation_information[[2]][i, -1])),
        base::names(imputation_information[[2]][i, -1])
      )
    }

    imputation_info <- check_impute(
      imputation_information = impute_info,
      current_data = current_data
    )

    data <- impute_success(
      current_data = current_data,
      imputation_info = imputation_info,
      dates = dates,
      id_col = data_cols$id_col,
      success_col = data_cols$success_col,
      prior_data = data,
      perfect_assignment = perfect_assignment,
      success_date_col = data_cols$success_date_col
    )
  }
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
    control_augment = control_augment
  )
  bandits[["bandit_stat"]][[(periods + 1)]] <- final_bandit[[1]]

  bandit_stats <- switch(algorithm,
    "Thompson" = {
      dplyr::bind_rows(bandits[["bandit_stat"]], .id = "period_number") |>
        dplyr::mutate(
          period_number = base::as.numeric(period_number),
          dplyr::across(-period_number, ~ dplyr::lead(., n = 1L, default = NA))
        ) |>
        dplyr::slice(base::seq_len(periods))
    },
    "UCB1" = {
      dplyr::bind_rows(bandits[["bandit_stat"]], .id = "period_number") |>
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


  assignment_probs <- dplyr::bind_rows(bandits[["assignment_prob"]], .id = "period_number") |>
    dplyr::mutate(period_number = base::as.numeric(period_number))



  return(list(
    final_data = data,
    bandits = bandit_stats,
    assignment_probs = assignment_probs
  ))
}
