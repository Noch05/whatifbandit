#' Prepping Data For Outcome Imputation

#' @name imputation_prep
#' @description Pre-computes the conditional probabilities of success and dates of success for each distinct
#' treatment block to impute them in [mab_simulation()] for those who get assigned new treatments.
#'
#' @inheritParams single_mab_simulation

#'
#' @returns A named list containing:
#' \item{original_summary: }{Object the same type as `data`,
#' which contains probability of success for each treatment block for each treatment period}
#' \item{dates_summary: }{Average success date for each treatment block at each treatment period}
#'
#' @seealso
#' *[impute_success()]
#' *[run_mab_trial()]
#'

imputation_prep <- function(data, whole_experiment, perfect_assignment, data_cols) {
  base::UseMethod("imputation_prep")
}
#-------------------------------------------------------------------------------

#' @method imputation_prep tbl_df
#' imputation Prep for Tibbles
#' @inheritParams imputation_prep

imputation_prep.tbl_df <- function(data, whole_experiment, perfect_assignment, data_cols) {
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
#' @method imputation_prep data.frame
#' imputation Prep for data.frames
#' @inheritParams imputation_prep

imputation_prep.data.frame <- function(data, whole_experiment, perfect_assignment, data_cols) {
  return(
    imputation_prep.tbl_df(
      data = tibble::as_tibble(data),
      data_cols = data_cols,
      whole_experiment = whole_experiment,
      perfect_assignment = perfect_assignment
    )
  )
}
#-------------------------------------------------------------------------------
