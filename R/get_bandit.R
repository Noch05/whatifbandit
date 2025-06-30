#' Calculate Multi-Arm Bandit Decision Based on Algorithm
#' @description Calculates the best treatment for a given period using either a UCB1 or Thompson Sampling Algorithm.
#'
#' @name get_bandit
#'
#' @inheritParams single_mab_simulation
#' @param past_results Data.frame containing summary of prior periods.
#' Created by [get_past_results()] components
#'
#'
#' @returns The bandit object for the given period.
#'
#' @seealso
#'* [run_mab_trial()]
#'* [get_past_results()]


get_bandit <- function(past_results, algorithm, conditions, current_period = NULL, control_augment = 0) {
  bandit <- switch(algorithm,
    "Thompson" = get_bandit.Thompson(past_results = past_results, conditions = conditions),
    "UCB1" = get_bandit.UCB1(past_results = past_results, conditions = conditions, current_period = current_period),
    rlang::abort("Invalid `algorithm`. Valid Algorithms: 'Thomspon', 'UCB1'")
  )
  bandit[[2]] <- augment_prob(assignment_probs = bandit[[2]], control_augment = control_augment)

  if (base::sum(bandit[[2]]) != 1) {
    bandit[[2]] <- bandits[[2]] / base::sum(bandit[[2]])
  }


  return(bandit)
}
#-------------------------------------------------------------------
#' @method get_bandit Thompson
#' @title Thompson Sampling Algorithm
#' @inheritParams get_bandit
#' @returns Named Numeric Vector of Posterior Probabilities

get_bandit.Thompson <- function(past_results, conditions) {
  bandit <- rlang::set_names(bandit::best_binomial_bandit(
    x = past_results$successes,
    n = past_results$n,
    alpha = 1,
    beta = 1
  ), base::sort(conditions))

  return(list(bandit, assignment_prob = bandit))
}
#-------------------------------------------------------------------
#' @method get_bandit UCB1
#' @title UCB1 Sampling Algorithm
#' @inheritParams get_bandit
#' @returns Data.frame containing UCB and Success Rate for each condition
#'

get_bandit.UCB1 <- function(past_results, conditions, current_period) {
  correction <- 1e-10

  if (inherits(past_results, "data.table")) {
    past_results[, ucb := success_rate + base::sqrt(
      (2 * base::log(current_period - 1)) / (n + correction)
    )]
    best_condition <- base::as.character(bandit[order(ucb)][1, mab_condition])
  } else {
    past_results <- past_results |>
      dplyr::mutate(
        ucb = success_rate + base::sqrt(
          (2 * base::log(current_period - 1)) / (n + correction)
        )
      )
    best_condition <- past_results |>
      dplyr::slice_max(order_by = ucb, n = 1, with_ties = FALSE) |>
      dplyr::ungroup() |>
      dplyr::pull(mab_condition)
  }

  assignment_probs <- rlang::set_names(rep(0, length(conditions)), conditions)
  assignment_probs[[best_condition]] <- 1

  return(invisible(list(past_results, assignment_probs)))
}

#' @name augment_prob
#' @title Control Augmentation for Treatment Assignment
#' @description
#' Adjusts Probabilities of Assignment to match a control augmentation framework.
#' If probability threshold is not meant, values are adjusted uniformly to augment the control probability.
#' @inheritParams single_mab_simulation
#' @param assignment_probs Named numeric vector; contains probabilities of
#' assignment with the control condition named "Control".
#' @returns Named numeric vector with updated probabilities

augment_prob <- function(assignment_probs, control_augment) {
  if (assignment_probs[["Control"]] < control_augment) {
    diff <- control_augment - assignment_probs[["Control"]]
    sub_from <- diff / base::length(conditions)

    assignment_probs[["Control"]] <- assignment_probs[["Control"]] + diff
    assignment_probs[!base::names(assignment_probs) %in% "Control"] <-
      assignment_probs[!base::names(assignment_probs) %in% "Control"] - sub_from
  }
  return(assignment_probs)
}
