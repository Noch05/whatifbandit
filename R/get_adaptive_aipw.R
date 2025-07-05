#' Compute Adaptive AIPW Estimates of Treatment Success
#' @name get_adaptive_aipw
#' @description Wrapper function around [get_iaipw()] and [adaptive_aipw()]. Computes Adaptive Augmented
#' Inverse Probability Estimator for each treatment following formulation in
#' \href{https://www.pnas.org/doi/pdf/10.1073/pnas.2014602118}{Hadad et. al (2021)}.
#'
#' @param data `final_data` object from [run_mab_trial()]. Contains results of Multi-Arm-Bandit Simulation
#' @param assignment_probs `assignment_probs` object from [run_mab_trial()]. Contains probability
#' of receiving each treatment at each treatment period in the simulated trial.
#' @param periods Numeric; number of treatment waves.
#' @inheritParams single_mab_simulation
#'
#'
#' @returns A named list containing:
#' \item{final_data}{`final_data` from [run_mab_trial()], updated with individual AIPW scores]}
#' \item{bandits}{Either the UCB1 statistics or Thompson Sampling posterior distributions.}
#' \item{estimates}{ object containing AIPW estimate and variance for each treatment}
#'
#' @seealso
#' * [get_iaipw()]
#' * [adaptive_aipw()]
#' * \href{https://www.pnas.org/doi/pdf/10.1073/pnas.2014602118}{Hadad et. al (2021)}





get_adaptive_aipw <- function(data, assignment_probs, periods,
                              conditions, algorithm, verbose) {
  data <- get_iaipw(
    data = data,
    assignment_probs = assignment_probs,
    mab = mab,
    conditions = conditions,
    algorithm = algorithm,
    periods = periods,
    verbose = verbose
  )

  estimates <- adaptive_aipw(
    mab = mab,
    periods = periods,
    conditions = conditions,
    algorithm = algorithm, verbose = verbose
  )

  results <- list(
    final_data = data,
    estimates = estimates
  )

  return(results)
}
