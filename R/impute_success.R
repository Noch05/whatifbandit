#' Imputing New Outcomes under MAB Trial
#' @name impute_success
#' @description Imputes Outcomes for the current treatment assignment period.
#' Uses[randomizr::block_and_cluster_ra] to impute success of the new treatments based on data from the original trial
#'
#' @param current_data Updated `data` object containing new treatments from [assign_treatments()] to impute outcomes for
#' @inheritParams run_mab_trial
#' @param prior_data `data` object from previous periods. Joined together at the end for the next iteration of the simulation.
#' @param imputation_info `data` object containing probabilities of success from the original experiment, to impute outcomes from.
#' Created by [imputation_prep()]
#' @param dates Named date vector; Contains average success date by treatment block to impute new success dates for
#' observations whose change in treatment changes their outcome from failure to success.
#' @inheritParams get_past_results
#' @inheritParams single_mab_simulation
#' @inheritParams cols
#'
#' @seealso
#'* [run_mab_trial()]
#'* [imputation_prep()]
#'* [randomizr::block_and_cluster_ra()]
#'* [randomizr::cluster_ra()]
impute_success <- function(current_data, imputation_info, id_col,
                           success_col, prior_data = NULL, perfect_assignment, dates = NULL,
                           success_date_col, current_period = NULL) {
  base::UseMethod("impute_success")
}
#' @inheritParams impute_success
#' @method impute_success tbl_df
#' @title [impute_success()] for tibbles
#'

impute_success.tbl_df <- function(current_data, imputation_info, id_col,
                                  success_col, prior_data, perfect_assignment, dates = NULL,
                                  success_date_col, current_period) {
  ## Imputing success randomly based on previously calculated Probabilities

  if (base::any(current_data$impute_req == 1, na.rm = TRUE)) {
    filtered_data <- current_data[current_data$impute_req == 1, ]

    blocks <- filtered_data$impute_block

    clusters <- filtered_data[[id_col$name]]


    imputations <- randomizr::block_and_cluster_ra(
      blocks = blocks,
      clusters = clusters,
      block_prob_each = imputation_info[, c("failure_rate", "success_rate")],
      num_arms = 2,
      conditions = c(0, 1),
      check_inputs = FALSE
    )

    # Joining Data Together and Returning
    filtered_data$mab_success <- imputations

    imputed <- dplyr::rows_update(current_data, filtered_data, by = id_col$name)

    imputed$mab_success <- base::ifelse(
      base::is.na(imputed$mab_success) & imputed$impute_req == 0, imputed[[success_col$name]], imputed$mab_success
    )
  } else {
    imputed <- current_data
    imputed$mab_success <- imputed[[success_col$name]]
  }

  if (!perfect_assignment) {
    imputed$new_success_date <- dplyr::case_when(
      imputed$impute_req == 0 | (imputed[[success_col$name]] == 0 & imputed$mab_success == 1) ~ imputed[[success_date_col$name]],
      imputed$mab_success == 1 & imputed[[success_col$name]] == 0 ~ dates[imputed$impute_block],
      imputed$mab_success == 0 | TRUE ~ base::as.Date(NA)
    )
  }
  data <- dplyr::rows_update(prior_data, imputed, by = id_col$name)
  return(data)
}
#-------------------------------------------------------------------------------
#' @inheritParams impute_success
#' @method impute_success data.frame
#' @title [impute_success()] for data.frames
impute_success.data.frame <- function(current_data, imputation_info, id_col,
                                      success_col, prior_data, perfect_assignment, dates = NULL,
                                      success_date_col, current_period) {
  return(
    impute_success.tbl_df(
      current_data = tibble::as_tibble(current_data),
      imputation_info = tibble::as_tibble(imputation_info),
      prior_data = tibble::as.tibble(prior_data),
      success_col = success_col,
      perfect_assignment = perfect_assignment,
      dates = dates,
      success_date_col = success_date_col,
      current_period = current_period
    )
  )
}
#-------------------------------------------------------------------------------
#' @inheritParams impute_success
#' @method impute_success data.table
#' @title [impute_success()] for data.tables
impute_success.data.table <- function(current_data, imputation_info, id_col,
                                      success_col, prior_data, perfect_assignment, dates = NULL,
                                      success_date_col, current_period) {
  ## Imputing success randomly based on previously calculated Probabilities

  if (current_data[impute_req == 1 & period_number == current_period, .N] > 0) {
    blocks <- current_data[impute_req == 1 & period_number == current_period, impute_block]
    clusters <- current_data[impute_req == 1 & period_number == current_period, base::get(id_col$name)]


    imputations <- randomizr::block_and_cluster_ra(
      blocks = blocks,
      clusters = clusters,
      block_prob_each = imputation_info[, .(failure_rate, success_rate)],
      num_arms = 2,
      conditions = c(0, 1),
      check_inputs = FALSE
    )

    # Joining Data Together and Returning
    current_data[impute_req == 1 & period_number == current_period, mab_success := imputations]
    current_data[impute_req == 0 & period_number == current_period, mab_success := base::get(success_col$name)]
  } else {
    current_data[period_number == current_period, mab_success := base::get(success_col$name)]
  }

  if (!perfect_assignment) {
    current_data[
      period_number == current_period,
      new_success_date := data.table::fcase(
        impute_req == 0 | (get(success_col$name) == 0 & mab_success == 1), get(success_date_col$name),
        mab_success == 1 & get(success_col$name) == 0, dates[impute_block],
        mab_success == 0 | TRUE, base::as.Date(NA)
      )
    ]
  }

  return(invisible(current_data))
}
