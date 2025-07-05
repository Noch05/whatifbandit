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
#' @param dates Named date vector; Contains average date by treatment block to impute new success dates for
#' observations whose change in treatment changes their outcome from failure to success.
#' @inheritParams single_mab_simulation
#' @inheritParams cols
#'
#' @seealso
#'* [run_mab_trial()]
#'* [imputation_prep()]
#'* [randomizr::block_and_cluster_ra()]
#'* [randomizr::cluster_ra()]


impute_success <- function(current_data, imputation_info, id_col,
                           success_col, prior_data, perfect_assignment, dates = NULL,
                           success_date_col) {
  ## Imputing success randomly based on previously calculated Probabilities
  ##

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

  # Recert Date for newly imputed success as average of recert date in the period from experimental data, grouped by original treatment
  # Used for judging cases when simulating lack of information due to time constraints, without this,
  # Those who changed treatment, and resulted in success when before were failures will not have a recert date
  #
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
