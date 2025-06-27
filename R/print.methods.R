#' Print Generics for Mab Object
#' @description
#' Custom Print Display for objects of mab class returned by [single_mab_simulation()].
#' @export
#' @param x "mab" class object created by [single_mab_simulation()]
#' @param ... further arguments passed to or from other methods
#' @method print mab
#' @returns Text Summary of the Settings used for the MAB trial
print.mab <- function(x, ...) {
  settings <- x$settings

  base::cat("Summary for MAB Procedure: \n ----------------------------------------------------- \n")

  base::cat("Data:               ", base::toupper(settings$data), "\n")
  base::cat("Bandit Algorithm:   ", settings$algorithm, "\n")
  base::cat("Perfect Assignment: ", settings$perfect_assignment, "\n")
  base::cat("Whole Experiment:   ", settings$whole_experiment, "\n")
  base::cat("Blocking Variables: ", settings$block_cols, "\n")
  base::cat("Assignment Method:  ", settings$assignment_method, "\n")
  base::cat("Period Length:      ", settings$period_length)
  if (settings$assignment_method == "Batch") {
    base::cat(" People\n")
  }
  if (settings$assignment_method == "Date") {
    base::cat(settings$time_unit)
    if (settings$period_length > 1) {
      base::cat("s\n")
    } else {
      base::cat("\n")
    }
  }

  base::cat("Total Periods:      ", length(x$bandits), "\n")
  base::cat("Prior Periods:      ", settings$prior_periods, "\n")
  base::cat("Conditions:         ", base::paste(settings$conditions, collapse = ", "), "\n")

  if (!base::is.null(settings$trials)) {
    base::cat("Trials Conducted:   ", settings$trials, "\n")
    base::cat("Keep Final Data:    ", settings$keep_data, "\n")
  }
  base::cat("----------------------------------------------------- \n")
}
