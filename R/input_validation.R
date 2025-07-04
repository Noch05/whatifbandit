#' @title Validates Inputs For [single_mab_simulation()] and [multiple_mab_simulation].
#' @name check_args
#'
#'
#' @description This function provides input validation,
#' checking to ensure that all required function arguments have been entered,
#' and that they do not conflict with one another. The goal is to provide the user
#' with informative error messages so they can quickly fix their usage of the function.
#'
#' @inheritParams single_mab_simulation
#' @inheritParams cols
#'
#' @returns No return value. Throws an error
#' if problems exist before running [single_mab_simulation]
#' or [multiple_mab_simulation()].
#'
#' @seealso
#' *[single_mab_simulation()]
#' *[multiple_mab_simulation()]
#'
#'
#'
check_args <- function(data,
                       assignment_method,
                       algorithm,
                       conditions,
                       prior_periods,
                       perfect_assignment,
                       whole_experiment,
                       blocking,
                       col_names,
                       time_unit,
                       period_length,
                       control_augment) {
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
  # Checking Arguments are properly provided
  if (assignment_method == "Date" && is.null(time_unit)) {
    rlang::abort("`time_unit` must be provided when assignment method is `Date`")
  }
  if (assignment_method %in% c("Date", "Batch") && is.null(period_length)) {
    rlang::abort("`period_length`, must be provided when Date or Batch assignment is used")
  }
  if (control_augment > 0 && !"Control" %in% names(conditions)) {
    rlang::abort("Condtions vector must have a at least one condition named 'Control'
    when control augmentation is used")
  }
  if ((period_length %% 1 != 0 || period_length > 0) && !is.null(period_length)) {
    rlang::abort("`period_length` must be a positive integer")
  }



  # Checking Column Proper Columns are Provided
  all_cols <- c("id", "success", "date", "month", "success_date", "assignment_date")
  required_cols <- all_cols[1:2]
  
  if(assignment_method == "Date") {
    if(time_unit == "Month") {
      required_cols <- c(required_cols, all_cols[3:4])
    }
    else {
      required_cols <- c(required_cols, all_cols[3])
    }
  }
  if (!pefect_assignment) {
    required_cols <- c(required_cols, all_cols[5:6])
  }
  non_required_cols <- base::setdiff(all_cols, required_cols)
  
  


    # Checking Column Existence

    cols <- base::list(
      id_col = rlang::enquo(id_col),
      date_col = rlang::enquo(date_col),
      condition_col = rlang::enquo(condition_col),
      success_col = rlang::enquo(success_col)
    )
  }

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
  return(name %in% base::names(data))
}


#'
