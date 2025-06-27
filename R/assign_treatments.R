#' Adaptively Assign Treatments in a Period
#'
#' @description Assigns new treatments for an assignment wave based on the Bandit object provided. If UCB1, the
#' treatment with the highest bound will be chosen for all observations, If Thompson, the probabilities will be used
#' to randomly assign treatments using [randomizr::block_and_cluster_ra()] or [randomizr::cluster_ra()] from the
#' [randomizr] package.
#'
#' @name assign_treatments
#'
#' @inheritParams single_mab_simulation
#' @inheritParams create_prior
#' @param bandit Bandit object from [mab_sample()], defines treatment assignment based on either Thompson or UCB1 algorithms
#' @param prior Vector of prior periods to use for calculation, passed by [create_prior()]
#'
#'
#' @returns An updated data frame object with the new treatment conditions. If this treatment is different
#' then from under the original experiment, they are labelled as imputation required.
#'
#' @seealso
#'* [run_mab_trial()]
#'* [mab_sample()]
#'* [randomizr::block_and_cluster_ra()]
#'* [randomizr::cluster_ra()]
#' @export



assign_treatments <- function(data, bandit, current_period, blocking,
                              algorithm, id_col, conditions, condition_col,
                              success_col) {
  data <- data |>
    dplyr::filter(period_number == current_period) |>
    dplyr::arrange({{ id_col }})

  if (algorithm == "Thompson") {
    if (blocking) {
      new_treatments <- randomizr::block_and_cluster_ra(
        clusters = dplyr::pull(data, {{ id_col }}),
        blocks = dplyr::pull(data, block),
        prob_each = bandit,
        conditions = conditions
      )
    } else if (!blocking) {
      new_treatments <- randomizr::cluster_ra(
        clusters = dplyr::pull(data, {{ id_col }}),
        prob_each = bandit,
        conditions = conditions
      )
    } else {
      base::stop("Invalid: Specify TRUE or FALSE for blocking")
    }

    new_treatments_df <- tibble::tibble(
      mab_condition = new_treatments,
      !!rlang::enquo(id_col) := dplyr::pull(data, {{ id_col }})
    )

    data <- dplyr::rows_update(data, new_treatments_df, by = names(new_treatments_df)[2]) |>
      dplyr::mutate(
        mab_condition = dplyr::if_else(
          is.na(mab_condition), {{ condition_col }}, mab_condition
        ),
        impute_req = dplyr::if_else(as.character({{ condition_col }}) != as.character(mab_condition), 1, 0)
      )
  } else if (algorithm == "UCB1") {
    best_condition <- bandit |>
      dplyr::slice_max(order_by = ucb, n = 1, with_ties = FALSE) |>
      dplyr::ungroup()

    data <- data |>
      dplyr::mutate(
        mab_condition = base::as.character(best_condition$mab_condition),
        impute_req = dplyr::if_else(
          as.character({{ condition_col }}) != as.character(mab_condition), 1, 0
        )
      )
  } else {
    base::stop("Specify valid algorithm: Thompson or UCB1")
  }
  return(data)
}
