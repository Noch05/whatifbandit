#' Print Generic For `mab`
#' @description
#' Custom Print Display for objects of `mab` class returned by [single_mab_simulation()].
#' @param x `mab` class object created by [single_mab_simulation()]
#' @param ... further arguments passed to or from other methods
#' @method print mab
#' @name print.mab
#' @returns Text summary of settings used for the Multi-Arm Bandit trial.
#' @export
print.mab <- function(x, ...) {
  print_mab(x)
  base::cat("----------------------------------------------------- \n")
}
#-------------------------------------------------------------------------------
#' Print Generic For `multiple.mab`
#' @description Custom Print Display for `multiple.mab`` objects returned by [multiple_mab_simulation()].
#' @method print multiple.mab
#' @param x `multiple.mab` class object
#' @param ... further arguments passed to or from other methods
#' @returns Text summary of settings used for the Multi-Arm Bandit trials.
#' @export
print.multiple.mab <- function(x, ...) {
  settings <- x$settings
  print_mab(x)
  base::cat("Trials Conducted:     ", settings$trials, "trials\n")
  base::cat("Keep Final Data:      ", settings$keep_data, "\n")
  base::cat("----------------------------------------------------- \n")
}
#-----------------------------------------------------------------------------

#' Print Helper for `mab` and `multiple.mab`
#' @description Common items for the print generics for `mab` and `multiple.ma`b classes
#' @name print_mab
#' @param mab `mab` or `multiple.mab` object to derive settings from
#' @returns Text summary of settings used for the Multi-Arm Bandit trial.
print_mab <- function(mab) {
  settings <- mab$settings

  base::cat("Summary for MAB Procedure: \n ----------------------------------------------------- \n")

  base::cat("Data:                 ", base::toupper(settings$data), "\n")
  base::cat("Bandit Algorithm:     ", settings$algorithm, "\n")
  base::cat("Control Augmentation: ", settings$control_augment, "\n")
  base::cat("Perfect Assignment:   ", settings$perfect_assignment, "\n")
  base::cat("Whole Experiment:     ", settings$whole_experiment, "\n")
  base::cat("Blocking Variables:   ", settings$block_cols, "\n")
  base::cat("Assignment Method:    ", settings$assignment_method, "\n")

  if (settings$assignment_method %in% c("Batch", "Date")) {
    base::cat("Period Length:        ", settings$period_length)
  }
  if (settings$assignment_method == "Batch") {
    base::cat(" People\n")
  }
  if (settings$assignment_method == "Date") {
    base::cat("", settings$time_unit)
    if (settings$period_length > 1) {
      base::cat("s\n")
    } else {
      base::cat("\n")
    }
  }

  base::cat("Total Periods:        ", length(mab$bandits), "\n")
  base::cat("Prior Periods:        ", settings$prior_periods, "\n")
  base::cat("Conditions:           ", base::paste(settings$conditions, collapse = ", "), "\n")
}
