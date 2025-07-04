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
                       control_augment,
                       verbose) {
  # Basic Checks for Data and algorithm
  if (base::is.null(data) || !base::is.data.frame(data)) {
    rlang::abort("Input 'data' must be a non-null data.frame.")
  }

  if (!algorithm %in% c("Thompson", "UCB1")) {
    rlang::abort("'algorithm' must be 'Thompson' or 'UCB1'.")
  }

  if (is.null(conditions) || !is.character(conditions)) {
    rlang::abort("`conditions`, must be provided as a character vector.")
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
    rlang::abort("`time_unit` must be provided when assignment method is `Date`.")
  }
  if (assignment_method %in% c("Date", "Batch") && is.null(period_length)) {
    rlang::abort("`period_length`, must be provided when Date or Batch assignment is used.")
  }
  if (control_augment > 0 && !"Control" %in% names(conditions)) {
    rlang::abort("Condtions vector must have a at least one condition named 'Control'
    when control augmentation is used.")
  }
  if ((period_length %% 1 != 0 || period_length < 0) && !is.null(period_length)) {
    rlang::abort("`period_length` must be a positive integer.")
  }

  if (is.numeric(prior_periods)) {
    if (prior_periods %% 1 != 0 || prior_periods < 0) {
      rlang::abort("`prior_periods` must be a positive integer or 'All'.")
    }
  } else {
    if (prior_periods != "All") {
      rlang::abort("`prior_periods` must be a positive integer or 'All'.")
    }
  }

  # Checking Column Proper Columns are Provided

  check_cols(
    data = data,
    assignment_method = assignment_method, time_unit = time_unit,
    perfect_assignment = perfect_assignment,
    col_names = col_names, verbose = verbose
  )
  if (blocking) {
    if (is.null(col_names$block)) {
      rlang::abort("block_cols must be provided when blocking = TRUE.")
    } else {
      for (col in col_names$block) {
        if (!col %in% names(data)) {
          rlang::abort(sprintf("`%s is not in the data, but was chosen as a block.", col))
        }
      }
    }
  }
  if (!blocking && !is.null(col_names$block) && verbose) {
    message("Blocking is FALsE, arguments passed to `block_cols` will be ignored.")
  }

  unique_ids <- length(unique(data[[col_names$data$id]]))

  if (unique_ids != nrow(data)) {
    rlang::abort(paste(col_names$data$id, "is not a unique identifier, a unique id for each observation is required"))
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
check_cols <- function(assignment_method, time_unit, perfect_assignment, col_names, data, verbose) {
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
    missing_input <- !col %in% names(col_names$data)

    if (missing_input) {
      rlang::abort(c(sprintf("Required column `%s` is not declared in `data_cols`.", col),
        "x" = paste0("reason: ", req_reasons[[col]])
      ))
    }
    missing_data <- !col_names$data[[col]] %in% names(data)

    if (missing_data) {
      rlang::abort(c(sprintf("Required column `%s` is not found in provided `data`.", col),
        "x" = paste0("reason: ", req_reasons[[col]])
      ))
    }
  }


  # Now handle non-required columns that are present but unnecessary
  if (verbose) {
    non_required_cols <- setdiff(all_cols, required_cols)
    non_req_reasons <- list(
      date = "assignment_method is not 'Date'",
      month = "time_unit is not 'Month'",
      success_date = "perfect_assignment is TRUE",
      assignment_date = "perfect_assignment is TRUE"
    )

    for (col in non_required_cols) {
      if (col %in% names(col_names$data)) {
        rlang::warn(c(
          "i" = sprintf(
            "`%s` is not required because %s. It will be ignored.",
            col, non_req_reasons[[col]]
          )
        ))
      }
    }
  }
}
