#' Imputing New Outcomes under MAB Trial
#' @name impute_success
#' @description Imputes Outcomes for the current treatment assignment period. Uses data from original RCT
#' and [randomizr] to randomly assign success of the new treatments under the MAB procedure based on the success rates
#' of each treatment during the original experiment. Runs in loop defined by [run_mab_trial()].
#'
#' @param data A data frame object created by [assign_treatments()],
#' containing the new treatment conditions and whose outcome needs to be imputed
#' @param imputation_info A data frame object created by [imputation_prep()]
#' contains the probabilities of success for each treatment condition, and block, if blocked
#' by service center, to be passed to [randomizr::block_and_cluster_ra()] for the
#' probabilities of imputation.
#' @param prior_data A data frame containing results from previous periods, to be joined together at the end,
#' and passed into the next loop of the assignment process defined by [run_mab_trial()].
#' @inheritParams single_mab_simulation
#'
#' @seealso
#'* [run_mab_trial()]
#'* [imputation_prep()]
#'* [randomizr::block_and_cluster_ra()]
#'* [randomizr::cluster_ra()]
#' @export


impute_success <- function(current_data, imputation_info, id_col,
                           success_col, prior_data, perfect_assignment, dates = NULL,
                           success_date_col) {
  ## Imputing success randomly based on previously calculated Probabilities
  ##

  if (base::any(current_data$impute_req == 1, na.rm = TRUE)) {
    filtered_data <- current_data |>
      dplyr::filter(impute_req == 1)

    blocks <- dplyr::pull(filtered_data, impute_block)

    clusters <- dplyr::pull(filtered_data, {{ id_col }})


    imputations <- randomizr::block_and_cluster_ra(
      blocks = blocks,
      clusters = clusters,
      block_prob_each = dplyr::select(imputation_info, failure_rate, success_rate),
      num_arms = 2,
      conditions = c(0, 1),
      check_inputs = FALSE
    )

    # Joining Data Together and Returning
    filtered_data <- filtered_data |>
      dplyr::mutate(mab_success = imputations)

    imputed <- dplyr::rows_update(current_data, filtered_data, by = rlang::as_name(rlang::enquo(id_col))) |>
      dplyr::mutate(mab_success = dplyr::if_else(base::is.na(mab_success) & impute_req == 0,
        {{ success_col }}, mab_success
      ))
  } else {
    imputed <- current_data |>
      dplyr::mutate(mab_success = {{ success_col }})
  }

  # Recert Date for newly imputed success as average of recert date in the period from experimental data, grouped by original treatment
  # Used for judging cases when simulating lack of information due to time constraints, without this,
  # Those who changed treatment, and resulted in success when before were failures will not have a recert date
  #
  if (!perfect_assignment) {
    imputed <- imputed |>
      dplyr::mutate(
        new_success_date = dplyr::case_when(
          impute_req == 0 | ({{ success_col }} == 1 & mab_success == 1) ~ {{ success_date_col }},
          mab_success == 1 & {{ success_col }} == 0 ~ dates[mab_condition],
          mab_success == 0 | TRUE ~ base::as.Date(NA)
        )
      )
  }

  data <- dplyr::rows_update(prior_data, imputed, by = rlang::as_string(rlang::ensym(id_col)))

  return(data)
}
