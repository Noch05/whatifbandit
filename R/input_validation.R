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
  character_args <- list(
    conditions = conditions,
  )

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
  if ((period_length %% 1 != 0 || period_length < 0) && !is.null(period_length)) {
    rlang::abort("`period_length` must be a positive integer")
  }
  if ((prior_periods %% 1 != 0 || prior_periods < 0 || prior_periods != "All") && !is.null(period_length)) {
    rlang::abort("`prior_periods` must be a positive integer or 'All'")
  }

  # Checking Column Proper Columns are Provided

  check_cols(
    assignment_method = assignment_method, time_unit = time_unit,
    perfect_assignment = perfect_assignment,
    col_names = col_names
  )

  unique_ids <- length(unique(data[[col_names$id]]))

  if (unique_ids != nrow(data)) {
    rlang::abort(paste(col_names$id, "is not a unique identifier, a unique id for each observation is required"))
  }
}


#--------------------------------------------------------------------------------------------------------------
#' @title Checking existence and declaration of columns
#' @name check_cols
#'
#'
#' @description Takes the user's settings as input, and checks the required columns against
#' which ones are provided, and throws in error if the user did not provide a required column, or
#' the column they provide is not present in their data.
#'
#' @inheritParams single_mab_simulation
#' @inheritParams cols
#'
#' @returns Throws an error if columns are not properly declared or in data
#'
#' @seealso
#' *[single_mab_simulation()]
#' *[check_args()]
#'
#'
check_cols <- function(assignment_method, time_unit, perfect_assignment, col_names, data) {
  # All possible columns
  all_cols <- c("id", "success", "date", "month", "success_date", "assignment_date")

  # Reason each column might be required
  req_reasons <- list(
    id = "it is always required",
    success = "it is always required",
    date = "assignment_method is 'Date'",
    month = "time_unit is 'Month'",
    success_date = "perfect_assignment is FALSE",
    assignment_date = "perfect_assignment is FALSE"
  )

  # Determine required columns based on settings
  required_cols <- c("id", "success")

  if (assignment_method == "Date") {
    required_cols <- c(required_cols, "date")
    if (time_unit == "Month") {
      required_cols <- c(required_cols, "month")
    }
  }
  if (!perfect_assignment) {
    required_cols <- c(required_cols, "success_date", "assignment_date")
  }

  # Check for missing required columns
  for (col in required_cols) {
    missing_input <- !col %in% col_names$data
    missing_data <- !col %in% names(data)

    if (missing_input || missing_data) {
      problems <- c(
        if (missing_input) "not declared in `data_cols`",
        if (missing_data) "not found in provided `data.frame`"
      )
      msg <- paste(
        sprintf(
          "`%s` is required because %s, but it is %s.",
          col,
          req_reasons[[col]],
          paste(problems, collapse = " and ")
        )
      )
      rlang::abort(msg)
    }
  }

  # Now handle non-required columns that are present but unnecessary
  non_required_cols <- setdiff(all_cols, required_cols)
  non_req_reasons <- list(
    date = "assignment_method is not 'Date'",
    month = "time_unit is not 'Month'",
    success_date = "perfect_assignment is TRUE",
    assignment_date = "perfect_assignment is TRUE"
  )

  for (col in non_required_cols) {
    if (col %in% col_names$data) {
      reason <- non_req_reasons[[col]]
      msg <- sprintf(
        "`%s` was provided, but is not required because %s. It will be ignored.",
        col, reason
      )
      message(msg)
    }
  }
}
