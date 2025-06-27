#' @title Validates Inputs For [single_mab_simulation()]
#' @name check_args
#'
#'
#' @description This function provides input validation for [single_mab_simulation()],
#' checking to ensure that all required function arguments have been entered,
#' and that they do not conflict with one another. The goal is to provide the user
#' with informative error messages so they can quickly fix their usage of the function.
#'
#' @inheritParams single_mab_simulation
#'
#' @returns No return value. Throws an error and stops execution
#' if problems exist before running [single_mab_simulation]
#'
#' @seealso
#' *[single_mab_simulation()]
#'
#'
#'
#' @export
check_args <- function(data, time_unit, prior_periods, period_length,
                       algorithm, whole_experiment, perfect_assignment,
                       blocking, block_cols = NULL, conditions,
                       date_col, month_col, id_col, condition_col,
                       success_col,
                       success_date_col = NULL, assignment_date_col = NULL,
                       verbose) {
  if (base::is.null(data) || !base::is.data.frame(data)) {
    stop("Input 'data' must be a non-null data.frame. Please provide a valid data.frame.")
  }
  if (time_unit == "Individual" && !base::is.null(period_length)) {
    warning("Individual assignment specified, 'period_length' will be ignored.")
  }
  if (!algorithm %in% c("Thompson", "UCB1")) {
    stop("'algorithm' must be Thompson or UCB1. Please provide a valid algorithm.")
  }
  if (!base::is.logical(verbose)) {
    stop("Argument 'verbose' must be logical (TRUE or FALSE).")
  }
  if (!base::is.logical(blocking)) {
    stop("Argument 'blocking' must be logical (TRUE or FALSE).")
  }
  if (!base::is.logical(whole_experiment)) {
    stop("Argument 'whole_experiment' must be logical (TRUE or FALSE).")
  }
  if (!base::is.logical(perfect_assignment)) {
    stop("Argument 'perfect_assignment' must be logical (TRUE or FALSE).")
  }
  if (!perfect_assignment && !check_col(data, col = {{ success_date_col }})) {
    stop("`success_date_col` must be provided when `perfect_assigment` is FALSE. Please Provide valid column.")
  }

  if (blocking && base::is.null(block_cols)) {
    stop("`block_cols` must be provided when blocking is TRUE. Please provid columns to block by.")
  }
  if (!blocking && !base::is.null(block_cols)) {
    warning("blocking is FALSE. `block_cols` will be ignored.")
  }
  if (!base::is.logical(verbose)) {
    stop("verbose must be logical (TRUE or FALSE).")
  }
  if (!perfect_assignment && !check_col(data, col = {{ assignment_date_col }})) {
    stop("`assignment_date_col` must be provided when `perfect_assigment` is FALSE. Please Provide valid column.")
  }
  if (perfect_assignment && (check_col(data, col = {{ success_date_col }}) ||
    check_col(data, col = {{ assignment_date_col }}))) {
    warning("'perfect_assignment' is TRUE; success_date and assignment_date cols will be ignored.")
  }

  if (!check_col(data, {{ id_col }})) {
    stop(sprintf(
      "`%s` not found in data.frame. Please specify a valid column",
      rlang::as_name(rlang::enquo(id_col))
    ))
  }
  if (!check_col(data, {{ date_col }})) {
    stop(sprintf(
      "`%s` not found in data.frame. Please specify a valid column",
      rlang::as_name(rlang::enquo(date_col))
    ))
  }

  if (!check_col(data, {{ condition_col }})) {
    stop(sprintf(
      "`%s` not found in data.frame. Please specify a valid column",
      rlang::as_name(rlang::enquo(condition_col))
    ))
  }
  if (!check_col(data, {{ success_col }})) {
    stop(sprintf(
      "`%s` not found in data.frame. Please specify a valid column",
      rlang::as_name(rlang::enquo(success_col))
    ))
  }

  distinct_data <- data |>
    dplyr::distinct({{ id_col }})

  if (nrow(data) != nrow(distinct_data)) {
    stop(sprintf(
      "`%s`` must be contain a unique ID for each observation. Please provide a valid column.",
      rlang::as_name(rlang::enquo(id_col))
    ))
  }

  if (!"Control" %in% base::names(conditions) || base::is.null(conditions) ||
    !base::is.character(conditions)) {
    stop("`conditions` must be a named character vector with the control condition named `Control`.
         Please provide a valid vector.")
  }
}
