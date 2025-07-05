#' Calculate Individual AIPW For Each Treatment Condition
#' @name get_iaipw
#' @description Calculates the individual Augmented Inverse Probability Weighted Estimate (AIPW) of treatment
#' success for each treatment condition provided.
#'
#'
#' @inheritParams get_adaptive_aipw
#' @inheritParams single_mab_simulation

#'
#'
#' @returns A data frame containing the data used in the MAB trial
#' with new columns corresponding to the individual AIPW estimate for each treatment condition, and
#' the probability of being assigned a given treatment condition.
#'
#' @seealso
#' * [run_mab_trial()]
#' * [get_adaptive_aipw()]
#' * [single_mab_simulation()]
#'
get_iaipw <- function(data, assignment_probs, periods, algorithm, conditions, verbose) {
  verbose_log(verbose, "Computing Individual AIPW Estimates")
  base::UseMethod("get_iaipw")
}
#-------------------------------------------------------------------------------

#'
#' @method get_iaipw tbl_df
#' @title
#' [get_iaipw()] for tibbles
#' @inheritParams get_iaipw

get_iaipw.tbl_df <- function(data, assignment_probs, periods, algorithm, conditions, verbose) {
  new_cols <- paste0("aipw_", conditions)
  data[new_cols] <- NA_real_


  prior_data <- data |>
    dplyr::group_by(period_number, mab_condition) |>
    dplyr::summarize(
      successes = sum(mab_success, na.rm = TRUE),
      trials = dplyr::n(),
      .groups = "drop"
    )
  names(assignment_probs) <- c("period_number", paste0(names(assignment_probs)[-1], "_assign_prob"))

  data <- base::expand.grid(
    period_number = base::seq_len(periods),
    mab_condition = conditions
  ) |>
    dplyr::left_join(prior_data, by = c("period_number", "mab_condition")) |>
    dplyr::mutate(dplyr::across(c(successes, trials), ~ tidyr::replace_na(.x, 0))) |>
    dplyr::arrange(mab_condition, period_number) |>
    dplyr::group_by(mab_condition) |>
    dplyr::mutate(
      cumulative_successes = dplyr::lag(base::cumsum(successes), default = 0),
      cumulative_trials = dplyr::lag(base::cumsum(trials), default = 0),
      prior_period_success_rate = dplyr::if_else(
        cumulative_trials > 0, cumulative_successes / cumulative_trials, 0
      )
    ) |>
    dplyr::select(period_number, mab_condition, prior_period_success_rate) |>
    tidyr::pivot_wider(
      names_from = mab_condition, values_from = "prior_period_success_rate",
      names_prefix = "prior_rate_"
    ) |>
    dplyr::right_join(data, by = "period_number") |>
    dplyr::select(tidyselect::all_of(names(data)), tidyselect::everything()) |>
    dplyr::left_join(assignment_probs, by = "period_number")

  for (condition in conditions) {
    verbose_log(verbose, paste0("Condition: ", condition))

    probability <- data[[paste0(condition, "_assign_prob")]]
    mhat <- data[[paste0("prior_rate_", condition)]]

    data[[paste0("aipw_", condition)]] <- base::ifelse(
      data$mab_condition == condition,
      (data$mab_success / probability) + (1 - (1 / probability)) * mhat,
      mhat
    )
  }


  check <- data |>
    dplyr::summarize(dplyr::across(dplyr::starts_with("aipw_"), ~ base::sum(base::is.na(.x)))) |>
    base::sum()

  if (check != 0) {
    base::warning(paste0(check, " Individual AIPW Scores are NA"))
  }
  return(data)
}
# ------------------------------------------------------------------------------
#' @method get_iaipw data.table
#' @title [get_iaipw()] for data.tables
#' @inheritParams get_iaipw
#'
get_iaipw.data.table <- function(data, assignment_probs, periods, algorithm, conditions, verbose) {
  return(0)
}
