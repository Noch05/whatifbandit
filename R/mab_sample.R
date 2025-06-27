#' Calculate Posterior/UCB1 for Treatment in a Period
#' @description Calculates the best treatment for a given period using either a UCB1 or Thompson Sampling Algorithm
#' and a vector of prior periods. Used to determine the assignment for a given wave. Used during [run_mab_trial()]
#' at each period. Allows for the simulation of perfect/imperfect information in the calculation based on the known successes
#' at the time of the calculation. For `tanf` assumed to be the `letter_sent_date` in each month.
#'
#' @name mab_sample
#'
#' @inheritParams single_mab_simulation
#' @inheritParams get_past_results
#' @inheritParams create_prior
#'
#'
#' @returns The bandit object for the given period. For Thompson, this is a named numeric vector
#' of the posterior probabilities. For UCB1, this is a data frame containing UCB1 calculation for
#' each treatment
#'
#' @seealso
#'* [run_mab_trial()]
#'* [get_past_results()]
#' @export




mab_sample <- function(data, algorithm, prior, current_period, perfect_assignment, conditions,
                       assignment_date_col, success_date_col) {
  past_results <- get_past_results(
    data = data, prior = prior,
    current_period = current_period,
    perfect_assignment = perfect_assignment,
    conditions = conditions,
    assignment_date_col = {{ assignment_date_col }},
    success_date_col = {{ success_date_col }}
  )

  if (algorithm == "Thompson") {
    bandit <- rlang::set_names(bandit::best_binomial_bandit(
      x = past_results$successes,
      n = past_results$n,
      alpha = 1,
      beta = 1
    ), base::sort(conditions))

    if (base::sum(bandit) != 1) {
      bandit <- bandit / base::sum(bandit)
    }
  } else if (algorithm == "UCB1") {
    correction <- 1e-10
    bandit <- past_results |>
      dplyr::mutate(ucb = success_rate + base::sqrt(
        (2 * base::log(current_period - 1)) / (n + correction)
      )) |>
      dplyr::arrange(dplyr::desc(ucb))
  } else {
    base::stop("Valid algorithm required: UCB1 or Thompson")
  }
  return(bandit)
}
