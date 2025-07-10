#' @title Validates Inputs For [single_mab_simulation()] and [multiple_mab_simulation()]
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
                       data_cols,
                       block_cols,
                       time_unit,
                       period_length,
                       control_augment,
                       verbose) {
  # Basic Checks for Data and algorithm
  if (base::is.null(data) || !base::is.data.frame(data)) {
    rlang::abort("Input 'data' must be a non-null data.frame.")
  }

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
    if (!is.logical(.x)) {
      rlang::abort(sprintf("`%s` must be logical (TRUE or FALSE).", .y))
    }
  })

  # Checking Arguments are properly provided
  if (assignment_method == "Date" && is.null(time_unit)) {
    rlang::abort("`time_unit` must be provided when assignment method is `Date`.")
  }

  if (assignment_method != "Date" && !is.null(time_unit)) {
    if (verbose) rlang::warn(c("i" = "`time_unit` is not required when assignment method is not `Date`. It will be ignored"))
  }
  if (assignment_method %in% c("Date", "Batch") && is.null(period_length)) {
    rlang::abort("`period_length`, must be provided when Date or Batch assignment is used.")
  }
  if (is.null(control_augment) || !is.numeric(control_augment) || control_augment < 0 || control_augment > 1) {
    rlang::abort(c("`control_augment` must be a non-null double between 0 and 1",
      "x" = paste0("You passed: ", control_augment)
    ))
  }

  if (control_augment > 0 && !"Control" %in% names(conditions)) {
    rlang::abort("Condtions vector must have a at least one condition named 'Control'
    when control augmentation is used.")
  }
  if ((period_length %% 1 != 0 || period_length < 0) && !is.null(period_length)) {
    rlang::abort(c("`period_length` must be a positive integer.",
      x = paste0("You passed: ", period_length)
    ))
  }

  if (is.numeric(prior_periods)) {
    if (prior_periods %% 1 != 0 || prior_periods < 0) {
      rlang::abort(c("`prior_periods` must be a positive integer or 'All'.",
        "x" = paste0("You passed: ", prior_periods)
      ))
    }
  } else {
    if (prior_periods != "All") {
      rlang::abort(c("`prior_periods` must be a positive integer or 'All'.",
        "x" = paste0("You passed: ", prior_periods)
      ))
    }
  }
  if (assignment_method == "Batch" && period_length > nrow(data)) {
    rlang::abort(c("`period_length` cannot be larger than data size",
      "x" = sprintf("You data has %d, and your batch size is %d", nrow(data), period_length)
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

  if (inherits(data, "data.table")) {
    unique_ids <- length(unique(data[, get(data_cols$id$name)]))
  } else {
    unique_ids <- length(unique(data[[data_cols$id$name]]))
  }


  if (unique_ids != nrow(data)) {
    rlang::abort(paste(data_cols$id$name, "is not a unique identifier, a unique id for each observation is required"))
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
check_cols <- function(assignment_method, time_unit, perfect_assignment, data_cols, data, verbose) {
  # All possible columns
  all_cols <- c("id_col", "success_col", "date_col", "month_col", "success_date_col", "assignment_date_col")

  # Reason each column might be required
  all_reasons <- list(
    id_col = "it is always required",
    success_col = "it is always required",
    date_col = "assignment_method is 'Date'",
    month_col = "time_unit is 'Month'",
    success_date_col = "perfect_assignment is FALSE",
    assignment_date_col = "perfect_assignment is FALSE"
  )

  # Determine required columns based on settings
  required_cols <- c("id_col", "success_col")

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
        "x" = paste0("reason: ", .y)
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
