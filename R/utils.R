#' @importFrom rlang .data
#' @importFrom rlang !!
#' @importFrom rlang !!!
#' @importFrom stats density
#' @importFrom data.table .N
#' @importFrom data.table :=
#' @importFrom data.table .I

utils::globalVariables(c(
  ".SD",
  "assignment_method",
  "avg",
  "bandit",
  "block",
  "count",
  "failure_rate",
  "impute_block",
  "impute_req",
  "known_success",
  "n",
  "n_success",
  "new_success_date",
  "period",
  "success_rate",
  "successes",
  "time_weights",
  "time_weights_sq",
  "treatment_block",
  "trials",
  "ucb",
  ".I",
  ".N",
  ":=",
  "..",
  "period_number",
  "mab_condition",
  "month_date",
  "current_date",
  "mab_success",
  "variance",
  "probs",
  "prior_period_success_rate",
  "estimator",
  "cumulative_trials",
  "cumulative_count",
  "condition",
  "cumulative_success",
  "cumulative_successes",
  "trial",
  ".",
  "SE_avg",
  "SE_empirical",
  "times_best",
  "estimate_avg",
  "assignment_type",
  "average_probability_of_success",
  "estimated_probability_of_success",
  "Treatment_Arm",
  "value",
  "standard_dev",
  "cum_n"
))

#-------------------------------------------------------------------------------
#' Column as a Named Vector
#' @name as_named_vec
#' @description
#' Converts `data.frame` column into a vector using another column as the names for the vector
#' @param df `data.frame` used.
#' @param val Column name of values
#' @param name Column value of names
#' @returns vector with values `val` and names `name`
#'
#' @keywords internal
as_named_vec <- function(df, val, name) {
  x <- df[[val]]
  names(x) <- df[[name]]
  x
}

#' Verbose Printer
#' @description Shorthand Function for checking `verbose` and then printing if TRUE
#' @name verbose_log
#' @param message The message to be printed to screen, as a string.
#' @param log Logical; Whether or not to print the message, this will always be
#' the `verbose` argument passed from higher functions.
#' @returns Text output of `message` to the console when `log = TRUE`. If
#' `log = FALSE`, returns nothing.
#' @keywords internal

verbose_log <- function(log, message) {
  if (log) {
    cat(message, "\n")
  }
}
#------------------------------------------
#------------------------------------------------------------------------------
#' Formula Parser
#' @description
#' Parsers the input formula for [mab_from_rct()]
#' @name formula_parse
#' @inheritParams mab_from_rct
#' @returns List of columns specified from formula.
#' @keywords internal

formula_parse <- function(formula) {
  formula <- as.character(formula)

  outcome <- formula[2]

  obc <- strsplit(formula[3], "\\+") |>
    lapply(trimws) |>
    unlist()

  condition_col <- obc[1]
  other_vars <- lapply(
    list(
      obc[grepl("block\\((.*?)\\)", obc)],
      obc[grepl("cluster\\((.*?)\\)", obc)]
    ),
    gather_args
  )

  parsed <- list(
    condition_col = condition_col,
    success_col = outcome,
    block_cols = block(other_vars[[1]][["args"]]),
    cluster_col = cluster(other_vars[[2]][["args"]])
  )

  return(parsed)
}
#' Gather Args
#' @description Helper for formula parsing. Parses the expression, and splits the function call from the arguments.
#' @param x String representing an `R` expression, like `"block(x1)"`.
#' @returns A list containing the function call, and the arguments so `"Block(x1)"` gets turned into a list
#' with elements `block, "x1"`.

gather_args <- function(x) {
  if (length(x) == 0) {
    return(list(NULL))
  }
  call <- rlang::parse_expr(x) |>
    as.list()

  args <- vapply(
    call[-1],
    rlang::as_label,
    character(1)
  )
  return(list(call = call[[1]], args = args))
}

# Helpers not requiring documentation, simply identity functions for block and cluster cases.

block <- function(...) {
  c(...)
}
cluster <- function(x) {
  x
}
