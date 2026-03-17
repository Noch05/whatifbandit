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
