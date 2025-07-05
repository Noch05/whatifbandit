#' Checking Imputation Info
#' @description
#' Ensures the Imputation Info in the current iteration of [run_mab_trial()],
#' contains all the info needed, important when blocking or using small assignment waves
#'
#' @name check_impute
#' @param current_data data.frame, contains data from the period currently being imputed
#' @param imputation_information data.frame created by [imputation_prep()].


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
