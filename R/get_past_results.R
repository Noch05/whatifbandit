#' Gather Past Results for Given Assignment Period
#' @name get_past_results
#' @description Part of the Multi-Arm Bandit Trial Pipeline. This function filters the data by
#' the prior periods, calculates the success and success rate by treatment group to be used to
#' assign treatments via a Multi-Arm Bandit Algorithm. Provides functionality for simulating
#' the lack of information during assignment periods.
#'
#' @inheritParams single_mab_simulation
#' @inheritParams create_prior
#' @param data Data.frame object containing appropriate columns created by [create_cutoff()], and
#' results of previous periods of the MAB procedure.
#' @param prior Numeric vector containing the prior periods to be used for the treatment assignment
#'
#' @returns A data.frame, containing the number of successes, success rate, and number of people for each
#' treatment condition.
#'
#' @seealso
#' *[run_mab_trial()]
#' *[single_mab_simulation()]
#' *[mab_sample()]
#' @export
#'
#'


get_past_results <- function(data, prior, current_period, perfect_assignment, assignment_date_col,
                             success_date_col, conditions) {
  if (!perfect_assignment) {
    current_date <- data |>
      dplyr::filter(period_number == current_period) |>
      dplyr::summarize(current_date = base::max({{ assignment_date_col }}, na.rm = TRUE)) |>
      dplyr::pull(current_date)

    past_results <- data |>
      dplyr::filter(period_number %in% prior) |>
      dplyr::mutate(known_success = dplyr::case_when(
        current_date >= {{ success_date_col }} ~ mab_success,
        current_date < {{ success_date_col }} ~ 0,
        base::is.na({{ success_date_col }}) ~ 0,
        TRUE ~ 0
      ))
  } else {
    past_results <- data |>
      dplyr::filter(period_number %in% prior) |>
      dplyr::mutate(known_success = mab_success)
  }

  past_results <- past_results |>
    dplyr::group_by(mab_condition) |>
    dplyr::summarize(
      successes = base::sum(known_success, na.rm = TRUE),
      success_rate = base::mean(known_success, na.rm = TRUE),
      n = base::length(known_success),
      .groups = "drop"
    ) |>
    dplyr::ungroup()

  if (base::nrow(past_results) != base::length(conditions)) {
    conditions_add <- base::setdiff(conditions, past_results$mab_condition)

    replace <- tibble::tibble(
      mab_condition = conditions_add, successes = 0,
      success_rate = 0, n = 0
    )

    past_results <- base::rbind(past_results, replace) |>
      dplyr::arrange(mab_condition)
  }
  return(past_results)
}
