#' @name impute_loop_prep
#' @title Outcome imputation preparation
#' @description
#' Prepares necessary data to be passed to[impute_success()] to
#' impute outcomes properly. Subsets pre-computations and adds the `impute_block`,
#' column to `current_data`
#' @inheritParams get_past_results
#' @inheritParams run_mab_trial
#' @inheritParams impute_success
#' @returns A named list containing:
#' \itemize{
#'
#' }

impute_loop_prep <- function(current_data, block_cols, imputation_information,
                             whole_experiment, blocking, perfect_assignment, current_period) {
  # Creating block for imputing

  if (inherits(current_data, "data.table")) {
    if (blocking) {
      current_data[,
        impute_block := do.call(paste, c(.SD, sep = "_")),
        .SDcols = c("mab_condition", block_cols$name)
      ]
    } else {
      current_data[, impute_block := mab_condition]
    }
  } else {
    if (blocking) {
      current_data$impute_block <- do.call(
        paste, c(current_data[, c("mab_condition", block_cols$name)], sep = "_")
      )
    } else {
      current_data$impute_block <- current_data$mab_condition
    }
  }


  if (whole_experiment) {
    impute_success <- imputation_information[[1]]
  } else {
    impute_success <- imputation_information[[1]][[current_period]]
  }
  if (!perfect_assignment) {
    dates <- rlang::set_names(
      base::as.Date(base::as.numeric(imputation_information[[2]][current_period, -1])),
      base::names(imputation_information[[2]][current_period, -1])
    )
  } else {
    dates <- NULL
  }

  impute_success <- check_impute(
    imputation_information = impute_success,
    current_data = current_data,
    current_period = current_period
  )


  return(list(
    current_data = current_data,
    impute_success = impute_success,
    impute_dates = dates
  ))
}
