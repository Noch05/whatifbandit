#' Create Prior Periods
#' @name create_prior
#' @description Used during [run_mab_trial()] to create a vector of prior periods dynamically.
#'
#' @inheritParams single_mab_simulation
#' @param current_period The current period of the simulation. Defined by loop structure inside [run_mab_trial()].
#'
#' @return Numeric vector containing the prior treatment periods to be used for UCB1 and Thompson algorithms to
#' assign treatments
#'
#' @seealso
#' *[run_mab_trial()]

create_prior <- function(prior_periods, current_period) {
  if (prior_periods == "All") {
    ## Looking at all the past periods

    prior <- base::seq_len(current_period - 1)
  } else if (is.numeric(prior_periods) &
    prior_periods >= current_period) {
    ## Looking at all past periods until prior is less then current period

    prior <- base::seq_len(current_period - 1)
  } else if (is.numeric(prior_periods) &
    prior_periods < current_period) {
    # returns x most recent periods, i.e. if prior is 3, and current is 6, returns 3:5

    prior <- base::seq(from = current_period - prior_periods, to = (current_period - 1), by = 1)
  } else {
    stop("Invalid Prior Cutoff, specify either a whole number or \"All\"")
  }

  return(prior)
}
