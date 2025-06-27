#' @importFrom rlang .data
#' @importFrom rlang !!
#' @importFrom rlang !!!
#' @importFrom data.table :=
#' @importFrom data.table .I
#' @importFrom data.table .N
utils::globalVariables(c(
  ":=", ".I", "period_number", "mab_success", "mab_condition",
  "month_date", "new_success_date,
                         mean_success_date, period, success_rate, time_weights,
                         time_weights_sq, treatment_block, ucb,
                         avg, block, failure_rate, impute_block, impute_req,
                         known_success"
))
NULL
