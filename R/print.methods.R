#' Print Generics for Mab Object
#' @description
#' Custom Print Display for objects of mab class returned by [single_mab_simulation()].
#' @export
#' @param x "mab" class object created by [single_mab_simulation()]
#' @param ... further arguments passed to or from other methods
#' @method print mab
#' @returns Text Summary of the Settings used for the MAB trial
print.mab <- function(x, ...) {
  print_mab(x)
  base::cat("----------------------------------------------------- \n")
}
#-------------------------------------------------------------------------------
#' Print Generic for Multiple.Mab Object
#' @description Custom Print Display for multiple.mab objects returned by [multiple_mab_simulation()].
#' @method print multiple.mab
#' @param x "multiple.mab" class object
#' @param ... further arguments passed to or from other methods
#' @returns Text Summary of Settings Used for the Multiple MAB Trials
#' @export
print.multiple.mab <- function(x, ...) {
  settings <- x$settings
  print_mab(x)
  base::cat("Trials Conducted:      ", settings$trials, " trials\n")
  base::cat("Keep Final Data:       ", settings$keep_data, "\n")
  base::cat("----------------------------------------------------- \n")
}
#-----------------------------------------------------------------------------

#' Print Helper for MAB and Multiple MAB Objects
#' @description Common items for the print generics for mab and multiple.mab classes
#' @name print_mab
#' @param mab MAB or Multiple MAB object to derive settings from
#' @returns Text Summary of Settings used for the Mab Trial
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
