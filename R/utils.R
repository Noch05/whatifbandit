#' @importFrom rlang .data
#' @importFrom rlang !!
#' @importFrom rlang !!!
#' @importFrom stats density
#' @importFrom data.table .N
#' @importFrom data.table :=
#' @importFrom data.table .I

utils::globalVariables(c(
  ".SD", "assignment_method", "avg", "bandit",
  "block", "count",
  "failure_rate", "impute_block", "impute_req",
  "known_success", "n", "n_success",
  "new_success_date", "period", "success_rate",
  "successes", "time_weights", "time_weights_sq",
  "treatment_block", "trials", "ucb",
  ".I", ".N", ":=", "period_number",
  "mab_condition", "month_date",
  "current_date", "success_rate", "failure_rate",
  "mab_success", "variance", "probs",
  "prior_period_success_rate", "estimator",
  "cumulative_trials", "cumulative_count",
  "condition", "cumulative_success", "cumulative_successes",
  "trial", ".", "variance_avg", "times_best", "estimate_avg"
))

NULL

#' Verbose Printer
#' @description Shorthand Function for checking `verbose` and then printing if TRUE
#' @name verbose_log
#' @param message The message to be printed to screen, as a string.
#' @param log Logical; Whether or not to print the message, this will always be
#' the `verbose` argument passed from higher functions.
#' @returns Text output of `message` to the console when `log` is TRUE. If
#' `log` is FALSE, returns nothing.
#' @noRd

verbose_log <- function(log, message) {
  if (log) {
    base::cat(message, "\n")
  }
}

#' Check Level
#' @description
#' Shorthand for Checking if the `level` argument in the S3 generic methods
#' is valid for a confidence interval.
#' @name check_level
#' @inheritParams plot.mab
#' @returns Throws an error if `level` is invalid, else does nothing.
#' @noRd
check_level <- function(level) {
  if (!is.numeric(level) || (level < 0 || level > 1)) {
    rlang::abort(c("`level` must be a number between 0 and 1",
      "x" = paste0("You passed: ", level)
    ))
  }
}

#' Check Estimator
#' @description
#' Shorthand for checking if the `estimator` passed to
#' [plot.mab()] and [plot.multiple.mab()] are valid.
#' @name check_estimator
#' @inheritParams plot.mab
#' @returns Throws an error if the argument is invalid; returns character vector
#' with the user's selection based on the argument.
#' @noRd
check_estimator <- function(estimator) {
  if (base::is.null(estimator)) {
    rlang::abort("Invalid Estimator: Valid Estimators are `both`, `AIPW`, and `Sample`")
  } else {
    estimator_arg <- switch(estimator,
      "both" = c("Sample", "AIPW"),
      "AIPW" = c("AIPW"),
      "Sample" = c("Sample"),
      rlang::abort("Invalid Estimator: Valid Estimators are `both`, `AIPW`, and `Sample`")
    )
  }
  return(estimator_arg)
}
