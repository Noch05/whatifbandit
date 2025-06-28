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
#' @inheritParams assign_treatments

#'
#' @returns A named list containing:
#' \item{Probabilities}{summary data frame of success probabilities by treatment condition and blocking condition.}
#' \item{Dates}{Success Dates by condition, to be used for imputing new dates of success}
#' #' @seealso
#' * [impute_success()]
#'* [run_mab_trial()]
#' @export



imputation_prep <- function(data, whole_experiment, success_col, perfect_assignment, success_date_col = NULL,
                            condition_col = NULL) {
  # Choosing Whether to use all the data from the experiment or only up to the current treatment period

  if (whole_experiment) {
    imputation_information <- data |>
      dplyr::group_by(treatment_block) |>
      dplyr::summarize(success_rate = base::mean({{ success_col }}, na.rm = TRUE), .groups = "drop") |>
      dplyr::mutate(failure_rate = 1 - success_rate)
  } else if (!whole_experiment) {
    summary <- data |>
      dplyr::group_by(period_number, treatment_block) |>
      dplyr::summarize(
        count = dplyr::n(),
        n_success = base::sum({{ success_col }})
      ) |>
      dplyr::ungroup()

    imputation_information <- lapply(2:base::max(data$period_number), function(i) {
      imputation_info <- summary |>
        dplyr::filter(period_number < i) |>
        dplyr::group_by(treatment_block) |>
        dplyr::summarise(success_rate = base::sum(n_success) / base::sum(count)) |>
        dplyr::mutate(failure_rate = 1 - success_rate)
    })
    imputation_information <- c(list(0), imputation_information)
  } else {
    rlang::abort("Specify Logical for `whole_experiment`")
  }
  if (!perfect_assignment) {
    dates <- data |>
      dplyr::group_by({{ condition_col }}, period_number) |>
      dplyr::summarize(mean_date = base::mean({{ success_date_col }}, na.rm = TRUE)) |>
      dplyr::rename(condition = {{ condition_col }})

    dates <- lapply(2:base::max(data$period_number), function(j) {
      dates <- dplyr::filter(dates, period_number == j)

      return(rlang::set_names(dates$mean_date, dates$condition))
    })
    dates <- c(list(0), dates)
  } else {
    dates <- NULL
  }

  imputation_information <- list(imputation_information, dates)
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

  missing_blocks <- stats::na.omit(
    base::setdiff(
      current_data$impute_block[current_data$impute_req == 1],
      imputation_information$treatment_block
    )
  )

  blocks_to_remove <- stats::na.omit(
    base::setdiff(
      imputation_information$treatment_block,
      current_data$impute_block[current_data$impute_req == 1]
    )
  )

  if (base::length(missing_blocks) > 0) {
    addition <- tibble::tibble(
      treatment_block = missing_blocks,
      success_rate = mean_rate
    ) |>
      dplyr::mutate(failure_rate = 1 - success_rate)

    imputation_information <- dplyr::bind_rows(imputation_information, addition)
  }

  if (base::length(blocks_to_remove) > 0) {
    imputation_information <- imputation_information |>
      dplyr::filter(!treatment_block %in% blocks_to_remove)
  }

  imputation_information <- imputation_information |>
    dplyr::distinct(treatment_block, .keep_all = TRUE) |>
    dplyr::arrange(treatment_block)

  return(imputation_information)
}
