#' Constructs `mab` and its other class variants
#' @name construct_mab
#' @description Simple constructor for proper `mab` subclasses as output
#' to [simulate_mab()] and [mab_from_rct()].
#' @param mab Named list output of [simulate_mab()] or [mab_from_rct()].
#' @param type Type of simulated trial, either `"rct"` or `"param"` to denote whether it was an rct re-simulation or an simulation form population parameters.
#' @param multi Logical; `TRUE` denotes multiple trials.
#' @returns Input `mab` with appropriate S3 class, restructured for output
#' @keywords internal

construct_mab <- function(mab, type, multi) {
  class <- if (multi) {
    c(paste0("multi_", type, "_mab"), "multi_mab")
  } else {
    c(paste0("single_", type, "_mab"), "single_mab")
  }
  structure(
    list(
      new_data = mab$final_data,
      bandit = list(
        statistic = mab$bandits,
        assignment_prob = mab$assignment_prob,
        assignment_quant = mab$assignment_quantities
      ),
      estimates = mab$estimates,
      config = list(args = mab$args, call = mab$cl, parallel = mab$furrr)
    ),
    class = c(class, ".mab", "list")
  )
}

#' Constructs `test` and its other class variances
#' @name construct_test
#' @description
#' Simple constructor for proper `test` subclasses as output from [joint_test()], [pairwise_test()].
#' @param test Named list outpout of [joint_test()] or [pairwise_test()]
NULL
