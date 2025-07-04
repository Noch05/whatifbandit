#' Prepping Data For Outcome Imputation

#' @name imputation_prep
#' @description This function prepares
#' to have values of success imputed for those who were switched treatment conditions
#' by [assign_treatments()]. Simply calculates the Probability of success based on
#' treatment condition, using original experimental data.
#'
#'
#'
#' @inheritParams single_mab_simulation

#'
#' @returns A named list containing:
#' \item{Probabilities}{summary data frame of success probabilities by treatment condition and blocking condition.}
#' \item{Dates}{Success Dates by condition, to be used for imputing new dates of success}
#' #' @seealso
#' * [impute_success()]
#'* [run_mab_trial()]
#' @export



imputation_prep <- function(data, whole_experiment, perfect_assignment, data_cols) {
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
      dplyr::group_by(!!data_cols$condition_col$sym, period_number) |>
      dplyr::summarize(mean_date = base::mean(!!data_cols$success_date_col$sym, na.rm = TRUE), .groups = "drop") |>
      tidyr::pivot_wider(names_from = data_cols$condition_col$name, values_from = "mean_date")
  } else {
    dates_summary <- NULL
  }

  imputation_information <- list(original_summary, dates_summary)

  return(imputation_information)
}


#' Checking Imputation Info
#' @description
#' Ensures the Imputation Info in the current iteration of [run_mab_trial()],
#' contains all the info needed, important when blocking or using small assignment waves
#'
#' @name check_impute
#' @param current_data data.frame, contains data from the period currently being imputed
#' @param imputation_information data.frame created by [imputation_prep()].
#'
check_impute <- function(imputation_information, current_data) {
  mean_rate <- base::mean(imputation_information$success_rate)
  current_blocks <- current_data$impute_block[current_data$impute_req == 1]
  imputation_blocks <- imputation_information$treatment_block

  missing_blocks <- stats::na.omit(
    base::setdiff(current_blocks, imputation_blocks)
  )

  blocks_to_remove <- stats::na.omit(
    base::setdiff(imputation_blocks, current_blocks)
  )

  if (base::length(missing_blocks) > 0) {
    addition <- tibble::tibble(
      treatment_block = missing_blocks,
      success_rate = mean_rate
    )
    addition$failure_rate <- 1 - addition$success_rate

    imputation_information <- dplyr::bind_rows(imputation_information, addition)
  }

  if (base::length(blocks_to_remove) > 0) {
    imputation_information <- imputation_information[!imputation_information$treatment_block %in% blocks_to_remove, ]
  }

  imputation_information <- imputation_information[!duplicated(imputation_information$treatment_block), ][order(imputation_information$treatment_block), ]


  return(imputation_information)
}
