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
#'




assign_treatments <- function(current_data, bandit, blocking = NULL,
                              algorithm, id_col, conditions, condition_col,
                              success_col) {
  current_data <- switch(algorithm,
    "Thompson" = assign_treatments.Thompson(
      current_data = current_data,
      bandit = bandit,
      id_col = {{ id_col }},
      blocking = blocking,
      conditions = conditions,
      condition_col = {{ condition_col }},
      success_col = {{ success_col }}
    ),
    "UCB1" = assign_treatments.UCB1(
      current_data = current_data,
      bandit = bandit,
      id_col = {{ id_col }},
      conditions = conditions,
      condition_col = {{ condition_col }},
      success_col = {{ success_col }}
    ),
    rlang::abort("Specify valid algorithm: Thompson or UCB1")
  )
  return(current_data)
}
#-------------------------------------------------------------------------------
#' @method assign_treatments Thompson
#' @title Assign New Treatments Based on Thompson Posteririor
#' @inheritParams assign_treatments

assign_treatments.Thompson <- function(current_data, bandit, blocking,
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
      prob_each = bandit,
      conditions = conditions
    )
  } else if (!blocking) {
    new_treatments <- randomizr::cluster_ra(
      clusters = clusters,
      prob_each = bandit,
      conditions = conditions
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
      mutate(
        mab_condition = new_treatments,
        impute_req = dplyr::if_else(
          base::as.character(mab_condition) != base::as.character({{ condition_col }}), 1, 0
        )
      )

    return(current_data)
  }
}
#-------------------------------------------------------------------------------
#' @method assign_treatments UCB1
#' @title Assign New Treatments Based on UCB1 Statistic
#' @inheritParams assign_treatments

assign_treatments.UCB1 <- function(current_data, bandit,
                                   algorithm, id_col, conditions, condition_col,
                                   success_col) {
  if (inherits(bandit, "data.table")) {
    best_condtion <- base::as.character(bandit[order(ucb)][1, mab_condition])

    current_data[, mab_conditon := best_condition][
      ,
      impute_rep := data.table::fifelse(
        base::as.character(base::get(condition_col)) != as.character(mab_condition),
        1, 0
      )
    ]
    return(invisible(current_data))
  } else {
    best_condition <- bandit |>
      dplyr::slice_max(order_by = ucb, n = 1, with_ties = FALSE) |>
      dplyr::ungroup() |>
      dplyr::pull(mab_condition)

    current_data <- current_data |>
      dplyr::mutate(
        mab_condition = base::as.character(best_condition),
        impute_req = dplyr::if_else(
          as.character({{ condition_col }}) != as.character(mab_condition), 1, 0
        )
      )
    return(current_data)
  }
}
