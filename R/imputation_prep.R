#' Prepping Data For Outcome Imputation

#' @name imputation_prep
#' @description Pre-computes the conditional probabilities of success and dates of success for each distinct
#' treatment block to impute them in [mab_simulation()] for those who get assigned new treatments.
#'
#' @inheritParams single_mab_simulation

#'
#' @returns A named list containing:
#' \itemize{
#' \item `success`: Object the same type as `data`,
#' which contains probability of success for each treatment block for each treatment period.
#' \item `dates`: Average success date for each treatment block at each treatment period.
#' }
#'
#' @seealso
#' *[impute_success()]
#' *[run_mab_trial()]
#' @keywords internal

imputation_prep <- function(data, whole_experiment, perfect_assignment, data_cols) {
  base::UseMethod("imputation_prep", data)
}
#-------------------------------------------------------------------------------

#' @method imputation_prep data.frame
#' @title imputation Prep for data.frames
#' @inheritParams imputation_prep
#' @noRd

imputation_prep.data.frame <- function(data, whole_experiment, perfect_assignment, data_cols) {
  # Choosing Whether to use all the data from the experiment or only up to the current treatment period

  if (whole_experiment) {
    original_summary <- data |>
      dplyr::group_by(treatment_block) |>
      dplyr::summarize(success_rate = base::mean(!!data_cols$success_col$sym, na.rm = TRUE), .groups = "drop") |>
      dplyr::mutate(failure_rate = 1 - success_rate)
  } else if (!whole_experiment) {
    original_summary <- data |>
      dplyr::group_by(period_number, treatment_block) |>
      dplyr::summarize(
        count = dplyr::n(),
        n_success = base::sum(!!data_cols$success_col$sym), .groups = "drop",
      ) |>
      dplyr::arrange(period_number, treatment_block) |>
      dplyr::group_by(treatment_block) |>
      dplyr::mutate(
        cumulative_count = dplyr::lag(base::cumsum(count), default = 0),
        cumulative_success = dplyr::lag(base::cumsum(n_success), default = 0),
        success_rate = dplyr::if_else(
          cumulative_count > 0, (cumulative_success / cumulative_count), 0
        )
      ) |>
      dplyr::ungroup() |>
      dplyr::select(period_number, treatment_block, success_rate) |>
      dplyr::mutate(failure_rate = 1 - success_rate) |>
      dplyr::group_split(period_number)
  } else {
    rlang::abort("Specify Logical for `whole_experiment`")
  }

  if (!perfect_assignment) {
    dates_summary <- data |>
      dplyr::group_by(treatment_block, period_number) |>
      dplyr::summarize(mean_date = base::mean(!!data_cols$success_date_col$sym, na.rm = TRUE), .groups = "drop") |>
      tidyr::pivot_wider(names_from = "treatment_block", values_from = "mean_date")
  } else {
    dates_summary <- NULL
  }

  imputation_information <- list(original_summary, dates_summary)

  return(imputation_information)
}
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#' @method imputation_prep data.table
#' @title
#' imputation Prep for data.tables
#' @inheritParams imputation_prep
#' @noRd

imputation_prep.data.table <- function(data, whole_experiment, perfect_assignment, data_cols) {
  if (whole_experiment) {
    original_summary <- data[, .(
      success_rate = base::mean(base::get(data_cols$success_col$name), na.rm = TRUE)
    ), by = treatment_block]
    original_summary[, failure_rate := 1 - success_rate]

    data.table::setorder(original_summary, treatment_block)
  } else if (!whole_experiment) {
    original_summary <- data[, .(count = .N, n_success = base::sum(
      base::get(data_cols$success_col$name)
    )), by = .(period_number, treatment_block)]

    data.table::setorder(original_summary, period_number, treatment_block)

    original_summary[, `:=`(
      cumulative_count = data.table::shift(base::cumsum(count),
        type = "lag",
        fill = 0
      ),
      cumulative_success = data.table::shift(base::cumsum(n_success),
        type = "lag",
        fill = 0
      )
    ), by = treatment_block]

    original_summary[, success_rate := data.table::fifelse(
      cumulative_count > 0, (cumulative_success / cumulative_count), 0
    )]

    original_summary <- original_summary[, .(period_number, treatment_block, success_rate)]

    original_summary[, failure_rate := 1 - success_rate]

    original_summary <- split(original_summary, by = "period_number")
    original_summary <- lapply(original_summary, \(x) x[, period_number := NULL])
  } else {
    rlang::abort("Specify Logical for `whole_experiment`")
  }

  if (!perfect_assignment) {
    dates_summary <- data.table::dcast(
      data[, .(period_number, treatment_block, base::get(data_cols$success_date_col$name))],
      formula = period_number ~ treatment_block, fun.aggregate = \(x) base::mean(x, na.rm = TRUE),
      value.var = "V3"
    )
  } else {
    dates_summary <- NULL
  }

  imputation_information <- list(success = original_summary, dates = dates_summary)

  return(imputation_information)
}
#-------------------------------------------------------------------------------
#
