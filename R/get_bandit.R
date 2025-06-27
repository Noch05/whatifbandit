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


get_bandit <- function(past_results, algorithm, conditions) {
  bandit <- switch(algorithm,
    "Thompson" = get_bandit.Thompson(past_results = past_results, conditions = conditions),
    "UCB1" = get_bandit.UCB1(past_results = past_results, conditions = conditions),
    rlang::abort("Invalid `algorithm`. Valid Algorithms: 'Thomspon', 'UCB1'")
  )
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

  if (base::sum(bandit) != 1) {
    bandit <- bandit / base::sum(bandit)
  }
  return(bandit)
}
#-------------------------------------------------------------------
#' @method get_bandit UCB1
#' @title UCB1 Sampling Algorithm
#' @inheritParams get_bandit
#' @returns Data.frame containing UCB and Success Rate for each condition
#'

get_bandit.UCB1 <- function(past_results, conditions) {
  correction <- 1e-10
  bandit <- past_results |>
    dplyr::mutate(
      ucb = success_rate + base::sqrt(
        (2 * base::log(current_period - 1)) / (n + correction)
      )
    )
  return(bandit)
}

#'
#'
#'
