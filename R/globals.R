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
