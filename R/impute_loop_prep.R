#' @name impute_loop_prep
#' @title Outcome imputation preparation
#' @description
#' Prepares necessary data to be passed to[impute_success()] to
#' impute outcomes properly. Subsets pre-computations and adds the `impute_block`,
#' column to `current_data`
#' @inheritParams get_past_results
#' @inheritParams run_mab_trial
#' @returns A named list containing:
#' \itemize{
#'
#' }

impute_loop_prep <- function(current_data, block_cols, imputation_information) {
  # Creating block for imputing
  if (blocking) {
    current_data$impute_block <- do.call(
      paste, c(current_data[, c("mab_condition", block_cols$name)], sep = "_")
    )
  } else {
    current_data$impute_block <- current_data$mab_condition
  }


  if (whole_experiment) {
    impute_info <- imputation_information[["success"]]
  } else {
    impute_info <- imputation_information[["success"]][[i]]
  }
  if (!perfect_assignment) {
    dates <- rlang::set_names(
      base::as.Date(base::as.numeric(imputation_information[["dates"]][i, -1])),
      base::names(imputation_information[["dates"]][i, -1])
    )
  }

  imputation_info <- check_impute(
    imputation_information = impute_info,
    current_data = current_data
  )
  return(list(
    current_data = current_data,
  ))
}
