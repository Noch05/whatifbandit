#' Constructs `mab` and its other class variants
#' @name construct_mab
#' @description Simple control flow for constructing the proper `mab` classes as output
#' to [simulate_mab()] and [mab_from_rct()]
#' @param mab Named list output of [simulate_mab()] or [mab_from_rct()].
#' @returns Input `mab` with appropriate S3 class

construct_mab <- function(mab) {
  new_mab <- if (mab$args$r > 1) {
    new_multi_mab
  }
}

new_mab <- function(
  final_data,
  bandits,
  assignment_probs,
  estimates,
  ipw_vcov,
  settings,
  subclass
) {
  structure(
    list(
      final_data = final_data,
      bandits = bandits,
      assignment_probs = assignment_probs,
      estimates = estimates,
      ipw_vcov = ipw_vcov,
      settings = settings
    ),
    class = c(subclass, "mab")
  )
}

new_multi_mab <- function(
  trials,
  estimates,
  assignment_quantities,
  settings,
  subclass
) {
  structure(
    list(
      trials = trials, # list of mab objects
      estimates = estimates,
      assignment_quantities = assignment_quantities,
      settings = settings
    ),
    class = c(subclass, "multi_mab")
  )
}
