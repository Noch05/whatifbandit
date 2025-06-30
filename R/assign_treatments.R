#' Adaptively Assign Treatments in a Period
#'
#' @description Assigns new treatments for an assignment wave based on the assignment probabilities provided.
#' Probabilities passed to [randomizr::block_and_cluster_ra()] or [randomizr::cluster_ra()] for random assignment.
#'
#' @name assign_treatments
#'
#' @inheritParams single_mab_simulation
#' @inheritParams create_prior
#' @param probs Named Numeric Vector; Probability of Assignment for each treatment condition.
#'
#' @returns An updated data frame object with the new treatment conditions. If this treatment is different
#' then from under the original experiment, they are labelled as imputation required.
#'
#' @seealso
#'* [run_mab_trial()]
#'* [mab_sample()]
#'* [randomizr::block_and_cluster_ra()]
#'* [randomizr::cluster_ra()]
#'




assign_treatments <- function(current_data, probs, blocking = NULL,
                              algorithm, id_col, conditions, condition_col,
                              success_col) {
  # Performing Randomized Treatment Assignment
  if (inherits(current_data, "data.table")) {
    if (blocking) {
      blocks <- current_data[, block]
    }
    clusters <- current_data[, get(rlang::as_name(rlang::enquo(id_col)))]
  } else {
    if (blocking) {
      blocks <- dplyr::pull(current_data, block)
    }
    clusters <- dplyr::pull(current_data, {{ id_col }})
  }


  if (blocking) {
    new_treatments <- randomizr::block_and_cluster_ra(
      clusters = clusters,
      blocks = blocks,
      prob_each = probs,
      conditions = conditions,
      check_inputs = FALSE
    )
  } else if (!blocking) {
    new_treatments <- randomizr::cluster_ra(
      clusters = clusters,
      prob_each = probs,
      conditions = conditions,
      check_inputs = FALSE
    )
  } else {
    rlang::abort("Invalid: Specify TRUE or FALSE for blocking")
  }


  if (inherits(current_data, "data.table")) {
    current_data[, mab_condition := new_treatments][
      , impute_req := data.table::fifelse(
        base::as.character(mab_condition) != base::as.character(get(rlang::as_name(rlang::enquo(condition_col)))),
        1, 0
      )
    ]

    return(invisible(current_data))
  } else {
    current_data <- current_data |>
      dplyr::mutate(
        mab_condition = new_treatments,
        impute_req = dplyr::if_else(
          base::as.character(mab_condition) != base::as.character({{ condition_col }}), 1, 0
        )
      )

    return(current_data)
  }
}
