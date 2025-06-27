#' Compute Adaptive AIPW Estimates of Treatment Success
#' @name get_adaptive_aipw
#' @description Wrapper function around [get_iaipw()] and [adaptive_aipw()]. Computes Adaptive Augmented
#' Inverse Probability Estimator for each treatment following formulation in
#' \href{https://www.pnas.org/doi/pdf/10.1073/pnas.2014602118}{Hadad et. al (2021)}.
#'
#' @param mab A named list that has been created by [run_mab_trial()]
#' @inheritParams single_mab_simulation
#'
#'
#' @returns A named list containing:
#' \item{final_data}{Data frame from [run_mab_trial[], updated with individual AIPW scores]}
#' \item{bandits}{Either the UCB1 statistics or Thompson Sampling posterior distributions.}
#' \item{estimates}{Data.frame containing AIPW estimate and variance for each treatment}
#'
#' @seealso
#' * [get_iaipw()]
#' * [adaptive_aipw()]
#' * \href{https://www.pnas.org/doi/pdf/10.1073/pnas.2014602118}{Hadad et. al (2021)}
#' @export





get_adaptive_aipw <- function(mab, conditions, algorithm, verbose) {
  if (verbose) {
    message("Calculating AIPW Estimates")
  }

  periods <- base::max(mab[[1]]$period_number)

  mab[[1]] <- get_iaipw(
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
    algorithm = algorithm
  )

  results <- list(
    final_data = mab[[1]],
    bandits = mab[[2]],
    estimates = estimates
  )

  return(results)
}
