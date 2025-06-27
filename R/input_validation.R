#' @title Validates Inputs For [single_mab_simulation()] and [multiple_mab_simulation].
#' @name check_args
#'
#'
#' @description This function provides input validation,
#' checking to ensure that all required function arguments have been entered,
#' and that they do not conflict with one another. The goal is to provide the user
#' with informative error base::messages so they can quickly fix their usage of the function.
#'
#' @inheritParams single_mab_simulation
#'
#' @returns No return value. Throws an error
#' if problems exist before running [single_mab_simulation]
#' or [multiple_mab_simulation()].
#'
#' @seealso
#' *[single_mab_simulation()]
#'
#'
#'
check_args <- function(data, time_unit = NULL, prior_periods, period_length,
                       algorithm, whole_experiment, perfect_assignment,
                       blocking, block_cols = NULL, conditions,
                       date_col, month_col, id_col, condition_col,
                       success_col,
                       success_date_col = NULL, assignment_date_col = NULL,
                       verbose, assignment_method) {
  # Basic Checks for Data and algorithm
  if (base::is.null(data) || !base::is.data.frame(data)) {
    rlang::abort("Input 'data' must be a non-null data.frame.")
  }

  if (!algorithm %in% c("Thompson", "UCB1")) {
    rlang::abort("'algorithm' must be Thompson or UCB1.")
  }

  # Checking Logical values


  logical_args <- base::list(
    verbose = verbose,
    blocking = blocking,
    whole_experiment = whole_experiment,
    perfect_assignment = perfect_assignment
  )

  for (i in base::seq_along(logical_args)) {
    if (!is.logical(logical_args[[i]])) {
      rlang::abort(sprintf("`%s` must be logical (TRUE or FALSE).", base::names(logical_args)[[i]]))
    }
  }

  # Checking Column Existence

  cols <- base::list(
    id_col = rlang::enquo(id_col),
    date_col = rlang::enquo(date_col),
    condition_col = rlang::enquo(condition_col),
    success_col = rlang::enquo(success_col)
  )

  for (i in base::seq_along(cols)) {
    if (!check_col(data, !!cols[[i]])) {
      rlang::abort(sprintf("`%s` not found in data.frame.", base::names(cols)[[i]]))
    }
  }

  # Additional Compatibility checks
  if (!perfect_assignment && !check_col(data, col = {{ success_date_col }})) {
    rlang::abort("`success_date_col` must be provided when `perfect_assigment` is FALSE.")
  }

  if (blocking && base::is.null(block_cols)) {
    rlang::abort("`block_cols` must be provided when blocking is TRUE.")
  }

  if (!perfect_assignment && !check_col(data, col = {{ assignment_date_col }})) {
    rlang::abort("`assignment_date_col` must be provided when `perfect_assigment` is FALSE.")
  }

  # Assignment Method Checks
  if (assignment_method == "Date" & !check_col(data, col = {{ date_col }})) {
    rlang::abort("`date_col` must be provided when assignment_method is \"Date\"")
  }

  if (assignment_method == "Date" && !check_col(data, col = {{ date_col }})) {
    rlang::abort("`date_col` must be provided when assignment_method is \"Date\"")
  }

  if (assignment_method %in% c("Date", "Batch") && (period_length < 0 || is.null(period_length) ||
    period_length %% 1 != 0)) {
    rlang::abort("`period_length` must be a non-null positive integer")
  }

  if (time_unit == "Month" && !(check_col(data, col = {{ month_col }}))) {
    rlang::abort("`month_col` must be provided when `time_unit` is 'Month'")
  }


  distinct_data <- data |>
    dplyr::distinct({{ id_col }})

  if (nrow(data) != nrow(distinct_data)) {
    rlang::abort(sprintf(
      "`%s`` must be contain a unique ID for each observation. Please provide a valid column.",
      rlang::as_name(rlang::enquo(id_col))
    ))
  }
  if (!"Control" %in% base::names(conditions) || base::is.null(conditions) ||
    !base::is.character(conditions)) {
    rlang::abort("`conditions` must be a named character vector with the control condition named `Control`.
         Please provide a valid vector.")
  }


  # Non Critical Messages -------------------------------------------------

  if (verbose) {
    if (assignment_method == "Date" && time_unit != "Month" && (check_col(data, col = {{ month_col }}))) {
      base::message("`time_unit` is not 'Month', `month_col` will be ignored.")
    }
    if (assignment_method %in% c("Batch", "Individual") && check_col(data, {{ date_col }})) {
      base::message("`assignment_method` is not 'Date', `date_col`, will be ignored.")
    }

    if (assignment_method == "Individual" && !base::is.null(period_length)) {
      base::message("Individual assignment specified, 'period_length' will be ignored.")
    }

    if (perfect_assignment && (check_col(data, col = {{ success_date_col }}) ||
      check_col(data, col = {{ assignment_date_col }}))) {
      base::message("'perfect_assignment' is TRUE; success_date and assignment_date cols will be ignored.")
    }

    if (!blocking && !base::is.null(block_cols)) {
      base::message("blocking is FALSE. `block_cols` will be ignored.")
    }
  }
}


#' @title Checking Existence of a Column in data.frame
#' @name check_col
#'
#'
#' @description Helper function. Checks whether the specified column
#' exists in the data provided.
#'
#' @param data input data.frame
#' @param col column to check if exists
#'
#' @returns TRUE or FALSE based on the existence of the column in the data.
#'
#' @seealso
#' *[single_mab_simulation()]
#' *[check_args()]
#'
#'
check_col <- function(data, col) {
  if (rlang::quo_is_null(rlang::enquo(col))) {
    return(FALSE)
  }
  name <- rlang::as_name(rlang::enquo(col))

  return(name %in% base::names(data))
}


#'
