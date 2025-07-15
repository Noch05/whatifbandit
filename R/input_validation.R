#' @title Validates Inputs For [single_mab_simulation()] and [multiple_mab_simulation()]
#' @name check_args
#' @description This function checks to ensure that all required arguments
#' have been properly passed to the function before continuing with the simulation. When
#' errors are thrown, user-friendly messages are provided to indicate which argument
#' was misspecified. Additionally, when `verbose` = TRUE, additional warning
#' messages may be shown if unnecessary arguments are passed.
#' @inheritParams single_mab_simulation
#' @inheritParams cols
#' @returns No return value. Throws an error
#' if an argument is missing or misspecified.
#' @seealso
#' *[single_mab_simulation()]
#' *[multiple_mab_simulation()]
#' @keywords internal
check_args <- function(data,
                       assignment_method,
                       algorithm,
                       conditions,
                       prior_periods,
                       perfect_assignment,
                       whole_experiment,
                       blocking,
                       data_cols,
                       block_cols,
                       time_unit,
                       period_length,
                       control_augment,
                       verbose) {
  # Basic Checks for Data and algorithm

  if (!algorithm %in% c("Thompson", "UCB1")) {
    rlang::abort(c("'algorithm' must be 'Thompson' or 'UCB1'.",
      "x" = paste0("You passed: ", algorithm)
    ))
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

  purrr::walk2(logical_args, names(logical_args), ~ {
    if (!is.logical(.x) || length(.x) != 1 || is.na(.x)) {
      rlang::abort(c(sprintf("`%s` must be logical (TRUE or FALSE).", .y),
        "x" = paste0("You Passed: ", .x)
      ))
    }
  })
  if (!assignment_method %in% c("Individual", "Batch", "Date")) {
    rlang::abort(c("Invalid `assignment_method`",
      "x" = paste0("you passed: ", assignment_method),
      "i" = "Valid methods are `Individual`, `Batch`, `Date`"
    ))
  }

  # Checking Column Proper Columns are Provided
  check_cols(
    data = data,
    assignment_method = assignment_method, time_unit = time_unit,
    perfect_assignment = perfect_assignment,
    data_cols = data_cols, verbose = verbose
  )
  if (blocking) {
    if (is.null(block_cols)) {
      rlang::abort("block_cols must be provided when blocking = TRUE.")
    } else {
      for (i in base::seq_along(block_cols$name)) {
        if (!block_cols$name[[i]] %in% names(data)) {
          rlang::abort(sprintf("`%s is not in the data, but was chosen as a block.", block_cols$name[[i]]))
        }
      }
    }
  }
  if (!blocking && !is.null(block_cols) && verbose) {
    rlang::warn(c(
      "i" = "Blocking is FALsE, arguments passed to `block_cols` will be ignored."
    ))
  }

  unique_ids <- length(unique(data[[data_cols$id$name]]))


  if (unique_ids != nrow(data)) {
    rlang::abort(paste(data_cols$id$name, "is not a unique identifier, a unique id for each observation is required"))
  }
  # Checking Assignment Method Arguments
  if (assignment_method == "Date") {
    if (is.null(time_unit) || length(time_unit) != 1 || isTRUE(is.na(time_unit))) {
      rlang::abort("`time_unit` must be provided when assignment method is `Date`.")
    }
    if (!time_unit %in% c("Day", "Week", "Month")) {
      rlang::abort(c("Invalid Time Unit",
        "x" = paste0("you passed: ", time_unit),
        "i" = "valid units are `Day`, `Month`, `Week`"
      ))
    }
    if (!lubridate::is.Date(data[[data_cols$date$name]])) {
      rlang::abort(c(
        "date_col must be dates",
        "x" = sprintf("you passed: %s ", typeof(data[[data_cols$date$name]])),
        "i" = "Consider parsing your data to dates"
      ))
    }
  }
  if (assignment_method %in% c("Batch", "Date")) {
    if (is.null(period_length)) {
      rlang::abort(c("`period_length`, must be provided when Date or Batch assignment is used."))
    }
    if (!is.numeric(period_length)) {
      rlang::abort(c("`period_length` must be a positive integer.",
        "x" = paste0("You passed: ", period_length)
      ))
    }
    if (period_length %% 1 != 0 || period_length < 0) {
      rlang::abort(c("`period_length` must be a positive integer.",
        "x" = paste0("You passed: ", period_length)
      ))
    }
  }

  if (!assignment_method %in% c("Batch", "Date")) {
    if (verbose) {
      if (!is.null(period_length)) {
        rlang::warn(c("i" = "`time_unit` is not required when assignment method is not `Date`. It will be ignored"))
      }
    }
  }

  # Checking Other Arguments
  if (is.null(control_augment) || !is.numeric(control_augment) || control_augment < 0 || control_augment > 1) {
    rlang::abort(c("`control_augment` must be a non-null double between 0 and 1",
      "x" = paste0("You passed: ", control_augment)
    ))
  }

  if (control_augment > 0 && !"Control" %in% names(conditions)) {
    rlang::abort("Conditions vector must have a at least one condition named 'Control'
    when control augmentation is used.")
  }

  if (is.numeric(prior_periods)) {
    if (prior_periods %% 1 != 0 || prior_periods <= 0) {
      rlang::abort(c("`prior_periods` must be a positive integer or 'All'.",
        "x" = sprintf("You passed: %g ", prior_periods)
      ))
    }
  } else if (prior_periods != "All" || is.na(prior_periods)) {
    rlang::abort(c("`prior_periods` must be a positive integer or 'All'.",
      "x" = paste0("You passed: ", prior_periods)
    ))
  }

  if (length(conditions) != length(unique(data[[data_cols$condition_col$name]]))) {
    rlang::abort(c("Conditions passed must be same length as number of unique conditions",
      "x" = sprintf("You passed a vector of length %d", length(conditions)),
      "x" = sprintf("Your data has %d unique treatments", length(unique(data[[data_cols$condition_col$name]])))
    ))
  }

  if (assignment_method == "Batch" && period_length > nrow(data)) {
    rlang::abort(c("`period_length` cannot be larger than data size",
      "x" = sprintf("You data has %d, and your batch size is %d", nrow(data), period_length)
    ))
  }
}




#--------------------------------------------------------------------------------------------------------------
#' @title Checking existence and declaration of columns
#' @name check_cols
#' @description Helper to [check_args()]. This function accepts the user's
#' settings for the Multi-Arm-Bandit trial, and checks whether columns in the data have been properly
#' specified based on these settings.
#'
#' @inheritParams single_mab_simulation
#' @inheritParams cols
#' @returns Throws an error if columns which are required have not been declared
#' or are not present in the data. Additionally throws warning messages,
#' if unnecessary columns have been provided.
#' @keywords internal
#'
check_cols <- function(assignment_method, time_unit, perfect_assignment, data_cols, data, verbose) {
  # All possible columns
  all_cols <- c("id_col", "success_col", "condition_col", "date_col", "month_col", "success_date_col", "assignment_date_col")

  # Reason each column might be required
  all_reasons <- list(
    id_col = "it is always required",
    success_col = "it is always required",
    condition_col = "it is always required",
    date_col = "assignment_method is 'Date'",
    month_col = "time_unit is 'Month'",
    success_date_col = "perfect_assignment is FALSE",
    assignment_date_col = "perfect_assignment is FALSE"
  )

  # Determine required columns based on settings
  required_cols <- c("id_col", "success_col", "condition_col")

  if (assignment_method == "Date") {
    required_cols <- c(required_cols, "date_col")
    if (time_unit == "Month") {
      required_cols <- c(required_cols, "month_col")
    }
  }
  if (!perfect_assignment) {
    required_cols <- c(required_cols, "success_date_col", "assignment_date_col")
  }
  req_reasons <- all_reasons[required_cols]

  # Check for missing required columns
  #
  purrr::walk2(required_cols, req_reasons, ~ {
    missing_input <- !.x %in% names(data_cols)
    if (missing_input) {
      rlang::abort(c(sprintf("Required column `%s` is not declared in `data_cols`.", .x),
        "x" = paste0("reason: ", .y)
      ))
    }

    missing_data <- !data_cols[[.x]]$name %in% names(data)
    if (missing_data) {
      rlang::abort(c(sprintf("Required column `%s` is not found in provided `data`.", .x),
        "x" = paste0("reason: ", .y),
        "x" = paste0("Your column: ", data_cols[[.x]]$name)
      ))
    }
  })


  # Now handle non-required columns that are present but unnecessary
  if (verbose) {
    non_required_cols <- setdiff(all_cols, required_cols)
    non_req_reasons <- list(
      date_col = "assignment_method is not 'Date'",
      month_col = "time_unit is not 'Month'",
      success_date_col = "perfect_assignment is TRUE",
      assignment_date_col = "perfect_assignment is TRUE"
    )
    non_req_reasons <- non_req_reasons[non_required_cols]

    purrr::walk2(non_req_reasons, non_req_reasons, ~ {
      if (.x %in% names(data_cols)) {
        rlang::warn(c("i" = sprintf(
          "`%s` is not required because %s. It will be ignored.",
          .x, .y
        )))
      }
    })
  }
}

#------------------------------------------------------------------------------
