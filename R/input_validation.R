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
                       verbose,
                       ndraws,
                       random_assign_prop) {
  # Checking Algorithm

  if (!algorithm %in% c("Thompson", "UCB1")) {
    rlang::abort(c("'algorithm' must be 'Thompson' or 'UCB1'.",
      "x" = paste0("You passed: ", algorithm)
    ))
  }

  if (!assignment_method %in% c("Individual", "Batch", "Date")) {
    rlang::abort(c("Invalid `assignment_method`",
      "x" = paste0("you passed: ", assignment_method),
      "i" = "Valid methods are `Individual`, `Batch`, `Date`"
    ))
  }
  # Checking Logical values
  check_logical(verbose, blocking, whole_experiment, perfect_assignment)

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
  if (verbose && !blocking && !is.null(block_cols)) {
    rlang::warn(c(
      "i" = "Blocking is FALSE, arguments passed to `block_cols` will be ignored."
    ))
  }


  # Checking Assignment Method Arguments
  check_assign_method(
    assignment_method = assignment_method,
    time_unit = time_unit,
    verbose = verbose,
    period_length = period_length
  )

  # Checking Numeric Arguments

  check_prop(control_augment, random_assign_prop)
  check_posint(ndraws, prior_periods)

  # Checking Data Structure
  check_data(
    data = data, data_cols = data_cols,
    assignment_method = assignment_method,
    period_length = period_length,
    time_unit = time_unit
  )

  # Checking Conditions Vector
  check_conditions(
    conditions = conditions,
    data = data,
    data_cols = data_cols,
    control_augment = control_augment
  )
}


#---------------------------------------------------------------------------------------
#'
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
#' @title Checking if Inputs are proper Logical Values (TRUE and FALSE)
#' @name check_logical
#' @returns Nothing; Throws an error if any input is not TRUE or FALSE
#' @description Helper to [check_args()]. This function accepts the user's
#' settings for logical values in the Multi-Arm-Bandit trial, and checks whether they are valid.
#' @param ... Arguments to check
#' @keywords internal
check_logical <- function(...) {
  args <- rlang::dots_list(..., .named = TRUE)
  purrr::iwalk(args, ~ {
    if (!is.logical(.x) || length(.x) != 1 || is.na(.x)) {
      rlang::abort(
        c(
          sprintf("`%s` must be a logical (TRUE or FALSE"), .y
        ),
        "x" = paste0("You Passed: ", .x)
      )
    }
  })
}
#--------------------------------------------------------------------------------
#' @title Checking if inputs are proportions
#' @name check_prop
#' @returns Nothing; Throws an error if any input is not a valid proportion
#' @description Helper to [check_args()]. This function accepts the user's
#' settings for proportion arguments and checks if they are valid.
#' @inheritParams check_logical
#' @keywords internal
check_prop <- function(...) {
  args <- rlang::dots_list(..., .named = TRUE)

  purrr::iwalk(args, ~ {
    if (is.null(.x) || !is.numeric(.x) || .x < 0 || .x > 1) {
      rlang::abort(c(
        sprintf("`%s` must be a non-null double between 0 and 1.", .y),
        "x" = paste0("You passed: ", .x)
      ))
    }
  })
  if (args$control_augment > 0 && args$random_assign_prop > 0) {
    rlang::warn(c("It is not recommended to use control augmentation with hybrid assignment;
                control augmentation only affects bandit assignments."))
  }
}
#-------------------------------------------------------------------------------
#' @title Checking if inputs are positive integers or a valid string
#' @name check_posint
#' @returns Nothing; Throws an error if any input is not a positive whole number or not
#' a valid string
#' @description Helper to [check_args()]. This function accepts the user's
#' settings for integer arguments and check if they are valid
#' @inheritParams check_logical
#' @keywords internal
check_posint <- function(...) {
  args <- rlang::dots_list(..., .named = TRUE)
  valid_strings <- list(
    ndraws = NULL,
    prior_periods = c("All")
  )

  purrr::pwalk(args, names(args), valid_strings, names(valid_strings) ~ {
    if (!posint(..1)) {
      rlang::abort(c(
        sprintf("`%s` must be a positive integer", ..2),
        "x" = sprintf("you passed: %g", ..1)
      ))
    }
    if (!is.numeric(..1) && (..1 %in% valid_strings[..4])) {
      rlang::abort(c(
        sprintf("`%s` must be a positive integer or '%s'", ..2, ..3),
        "x" = paste0("You passed", ..1)
      ))
    }
  })
}
posint <- function(x) {
  if (is.numeric(x) && (x >= 0 || ..1 %% x == 0)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}
#--------------------------------------------------------------------------------
#' @title Checking if conditions vector is proper
#' @name check_conditions
#' @returns Nothing; Throws an error if the conditions vector does not meet the
#' requirements for the user's specified settings.
#' @description Helper to [check_args()]. This function accepts the conditions
#' vector and checks whether it is valid.
#' @inheritParams single_mab_simulation
#' @keywords internal
check_conditions <- function(conditions, data, data_cols, control_augment) {
  if (is.null(conditions) || !is.character(conditions)) {
    rlang::abort("`conditions` must be provided as a character vector.")
  }

  if (control_augment > 0 && !"Control" %in% names(conditions)) {
    rlang::abort("The `conditions` vector must one element named 'Control' when control augmentation is used.")
  }

  actual_conditions <- unique(data[[data_cols$condition_col$name]])
  if (length(conditions) != length(actual_conditions)) {
    rlang::abort(c(
      "The number of `conditions` must match the number of unique treatment groups in the data.",
      "x" = sprintf("You passed a vector of length %d", length(conditions)),
      "x" = sprintf("Your data has %d unique treatments", length(actual_conditions))
    ))
  }
}
#-------------------------------------------------------------------------------
#' @title Checking for valid input data
#' @name check_data
#' @returns Nothing; Throws an error if the data does not meet the specifications
#' of the trial based on user settings.
#' @description Helper to [check_args()]. This function accepts the data and checks
#' whether it has Unique ID's and a valid date type `date_col`.
#' @inheritParams single_mab_simulation
#' @keywords internal
check_data <- function(data, data_cols, assignment_method, period_length, time_unit) {
  unique_ids <- length(unique(data[[data_cols$id$name]]))
  if (unique_ids != nrow(data)) {
    rlang::abort(paste(data_cols$id$name, "is not a unique identifier; a unique ID for each observation is required."))
  }
  if (assignment_method == "Date" && !lubridate::is.Date(data[[data_cols$date$name]])) {
    rlang::abort(c(
      "`date_col` must be of type Date.",
      "x" = sprintf("You passed: %s", typeof(data[[data_cols$date$name]])),
      "i" = "Consider parsing your data to dates with `lubridate::parse_date_time()`."
    ))
  }
  if (assignment_method == "Batch" && period_length > nrow(data)) {
    rlang::abort(c("`period_length` cannot be larger than data size",
      "x" = sprintf("You data has %d, and your batch size is %d", nrow(data), period_length)
    ))
  }
  if (assignment_method == "Date") {
    unit <- switch(time_unit,
      "Day" = lubridate::days(1),
      "Month" = months(1),
      "Week" = lubridate::weeks(1)
    )

    data_interval <- lubridate::interval(
      min(data[[data_cols$date_col$name]]), max(data[[data_cols$date_col$name]])
    ) / unit

    if (period_length > data_interval) {
      rlang::abort(c("`period_length` cannot be larger the date range of your data",
        "x" = sprintf(
          "Your period length is %d %ss but your data only covers %d %ss",
          period_length, tolower(time_unit), data_interval, tolower(time_unit)
        )
      ))
    }
  }
}
# ----------------------------------------------------------------------------
#' @title Checking for valid assignment methods
#' @name check_assign_method
#' @returns Nothing; Throws an error if the user is missing necessary arguments to
#' assign treatments or passes invalid ones
#' @description Helper to [check_args()]. This function accepts arguments relating
#' to how treatment waves are assigned, and checks if they are valid, and if all
#' supporting arguments are passed as necessary the data and checks
#' whether it has Unique ID's and a valid date type `date_col`.
#' @inheritParams single_mab_simulation
#' @keywords internal
check_assign_method <- function(assignment_method, time_unit, verbose, period_length) {
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
  }
  if (assignment_method %in% c("Batch", "Date")) {
    if (is.null(period_length)) {
      rlang::abort(c("`period_length`, must be provided when Date or Batch assignment is used."))
    }
    if (!posint(period_length)) {
      rlang::abort(c("`period_length` must be a positive integer.",
        "x" = paste0("You passed: ", period_length)
      ))
    }
  }

  if (verbose && !assignment_method %in% c("Batch", "Date") && !is.null(period_length)) {
    rlang::warn(c("i" = "`time_unit` is not required when assignment method is not `Date`. It will be ignored"))
  }
}
