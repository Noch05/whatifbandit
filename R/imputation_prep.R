#' Prepping Data For Outcome Imputation

#' @name imputation_prep
#' @description Called Internally by [run_mab_trial()], this function prepares
#' to have values of success imputed for those who were switched treatment conditions
#' by [assign_treatments()]. Simply calculates the Probability of success based on
#' treatment condition, using original experimental data.
#' Works to minimize errors from [randomizr::block_and_cluster_ra], when blocking by service center is used.
#'
#'
#'
#' @inheritParams single_mab_simulation
#' @inheritParams assign_treatments
#' @param current_data data.frame, contains data from the period currently being imputed
#'
#' @returns A summary data frame of success probabilities by treatment condition and blocking condition, passed to
#' [impute_success()]
#' @seealso
#' * [impute_success()]
#'* [run_mab_trial()]
#' @export



imputation_prep <- function(data, whole_experiment, current_period, success_col,
                            current_data) {
  # Choosing Whether to use all the data from the experiment or only up to the current treatment period

  if (whole_experiment) {
    imputation_information <- data
  } else if (!whole_experiment) {
    imputation_information <- data |>
      dplyr::filter(period_number %in% base::seq_len(current_period - 1))
  } else {
    base::stop("Specify logical for whole_experiment")
  }

  imputation_information <- imputation_information |>
    dplyr::group_by(treatment_block) |>
    dplyr::summarize(success_rate = base::mean({{ success_col }}, na.rm = TRUE), .groups = "drop") |>
    dplyr::mutate(failure_rate = 1 - success_rate)

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
