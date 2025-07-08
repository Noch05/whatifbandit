#' Checking Imputation Info
#' @description
#' Ensures the Imputation Info in the current iteration of [run_mab_trial()],
#' contains all the info needed, important when blocking or using small assignment waves
#'
#' @name check_impute
#' @inheritParams get_past_results
#' @inheritParams impute_success
#' @param imputation_information `success` element of the `imputation_information`
#' list created by [imputation_prep()].
check_impute <- function(imputation_information, current_data, current_period) {
  base::UseMethod("check_impute")
}
#' @method check_impute tbl_df
#' @title [check_impute()] for tibbles
#' @inheritParams check_impute

check_impute.tbl_df <- function(imputation_information, current_data, current_period) {
  mean_rate <- base::mean(imputation_information$success_rate)

  current_blocks <- stats::na.omit(current_data$impute_block[current_data$impute_req == 1])
  imputation_blocks <- stats::na.omit(imputation_information$treatment_block)

  missing_blocks <- base::setdiff(current_blocks, imputation_blocks)

  blocks_to_remove <- base::setdiff(imputation_blocks, current_blocks)

  if (base::length(missing_blocks) > 0) {
    addition <- tibble::tibble(
      treatment_block = missing_blocks,
      success_rate = mean_rate,
      failure_rate = 1 - mean_rate
    )

    imputation_information <- dplyr::bind_rows(imputation_information, addition)
  }

  if (base::length(blocks_to_remove) > 0) {
    imputation_information <- imputation_information[!imputation_information$treatment_block %in% blocks_to_remove, ]
  }

  imputation_information <- imputation_information[!duplicated(imputation_information$treatment_block), ][order(imputation_information$treatment_block), ]


  return(imputation_information)
}
#-------------------------------------------------------------------------------
#' @method check_impute data.frame
#' @title [check_impute()] for data.frames
#' @inheritParams check_impute
check_impute.data.frame <- function(imputation_information, current_data) {
  return(
    check_impute.tbl_df(
      tibble::as_tibble(imputation_information),
      tibble::as_tibble(current_data)
    )
  )
}
#' @method check_impute data.table
#' @title [check_impute()] for data.tables
#' @inheritParams check_impute
check_impute.data.table <- function(imputation_information, current_data, current_period) {
  mean_rate <- base::mean(imputation_information$success_rate)

  current_blocks <- stats::na.omit(current_data[impute_req == 1, impute_block])

  imputation_blocks <- stats::na.omit(imputation_information$treatment_block)

  missing_blocks <- base::setdiff(current_blocks, imputation_blocks)

  blocks_to_remove <- base::setdiff(imputation_blocks, current_blocks)

  if (base::length(missing_blocks) > 0) {
    addition <- data.table::data.table(
      treatment_block = missing_blocks,
      success_rate = mean_rate,
      failure_rate = 1 - mean_rate
    )

    imputation_information <- data.table::rbindlist(list(imputation_information, addition))
  }

  if (base::length(blocks_to_remove) > 0) {
    imputation_information <- imputation_information[!treatment_block %in% blocks_to_remove, ]
  }

  imputation_information <- imputation_information[!duplicated(treatment_block)]
  data.table::setorder(imputation_information, treatment_block)

  return(invisible(imputation_information))
}
