#' @title Validates Inputs For [single_mab_simulation()] and [multiple_mab_simulation()]
#' @name validate_inputs
#' @description This function checks to ensure that all required arguments
#' have been properly passed to the function before continuing with the simulation. When
#' errors are thrown, user-friendly messages are provided to indicate which argument
#' was misspecified. Additionally, when `verbose` = TRUE, additional warning
#' messages may be shown if unnecessary arguments are passed.
#' @inheritParams single_mab_simulation
#' @inheritParams cols
#' @returns Throws an error if an argument is missing or misspecified.
#' @seealso
#' * [single_mab_simulation()]
#' * [multiple_mab_simulation()]
#' @keywords internal
validate_inputs <- function(
  data,
  assignment_method,
  algorithm,
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
  random_assign_prop
) {
  # Checking Algorithm

  if (!algorithm %in% c("thompson", "ucb1")) {
    rlang::abort(c(
      "'algorithm' must be 'thompson' or 'ucb1'.",
      "x" = paste0("You passed: ", algorithm)
    ))
  }

  if (!assignment_method %in% c("individual", "batch", "date")) {
    rlang::abort(c(
      "Invalid `assignment_method`",
      "x" = paste0("you passed: ", assignment_method),
      "i" = "Valid methods are `individual`, `batch`, `date`"
    ))
  }
  # Checking Logical values
  check_logical(verbose, blocking, whole_experiment, perfect_assignment)

  # Checking Column Proper Columns are Provided
  check_cols(
    data = data,
    assignment_method = assignment_method,
    time_unit = time_unit,
    perfect_assignment = perfect_assignment,
    data_cols = data_cols,
    verbose = verbose
  )
  if (blocking) {
    if (is.null(block_cols)) {
      rlang::abort("block_cols must be provided when blocking = TRUE.")
    } else {
      for (i in base::seq_along(block_cols$name)) {
        if (!block_cols$name[[i]] %in% names(data)) {
          rlang::abort(sprintf(
            "`%s is not in the data, but was chosen as a block.",
            block_cols$name[[i]]
          ))
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
    data = data,
    data_cols = data_cols,
    assignment_method = assignment_method,
    period_length = period_length,
    time_unit = time_unit,
    perfect_assignment
  )
}


#---------------------------------------------------------------------------------------
#'
#' @title Checking existence and declaration of columns
#' @name check_cols
#' @description Helper to [validate_inputs()]. This function accepts the user's
#' settings for the Multi-Arm-Bandit trial, and checks whether columns in the data have been properly
#' specified based on these settings.
#' @inheritParams single_mab_simulation
#' @inheritParams cols
#' @returns Throws an error if columns which are required have not been declared
#' or are not present in the data, or are the wrong primitive data type. Additionally throws warning messages,
#' if unnecessary columns have been provided, only when `verbose` is TRUE.
#' @keywords internal
#'
check_cols <- function(
  assignment_method,
  time_unit,
  perfect_assignment,
  data_cols,
  data,
  verbose
) {
  # All possible columns
  all_cols <- c(
    "id_col",
    "success_col",
    "condition_col",
    "date_col",
    "month_col",
    "success_date_col",
    "assignment_date_col"
  )

  # Reason each column might be required
  all_reasons <- list(
    id_col = "it is always required",
    success_col = "it is always required",
    condition_col = "it is always required",
    date_col = "assignment_method is 'date'",
    month_col = "time_unit is 'month'",
    success_date_col = "perfect_assignment is FALSE",
    assignment_date_col = "perfect_assignment is FALSE"
  )
  data_types <- c(
    "numeric",
    "logical",
    "integer",
    "character",
    "factor",
    "Date",
    "POSIXt"
  )
  test_funcs <- c(
    is.numeric,
    is.logical,
    is.character,
    is.factor,
    lubridate::is.Date,
    lubridate::is.POSIXt
  )
  required_types <- list(
    id_col = list(classes = data_types, tests = test_funcs),
    success_col = list(classes = data_types[1:3], tests = test_funcs[1:2]),
    condition_col = list(classes = data_types[1:5], tests = test_funcs[1:4]),
    date_col = list(classes = data_types[6:7], tests = test_funcs[5:6]),
    month_col = list(
      classes = data_types[c(1, 3, 4, 5)],
      tests = test_funcs[c(1, 3, 4)]
    ),
    success_date_col = list(classes = data_types[6:7], tests = test_funcs[5:6]),
    assignment_date_col = list(
      classes = data_types[6:7],
      tests = test_funcs[5:6]
    )
  )

  # Determine required columns based on settings
  required_cols <- c("id_col", "success_col", "condition_col")

  if (assignment_method == "date") {
    required_cols <- c(required_cols, "date_col")
    if (time_unit == "month") {
      required_cols <- c(required_cols, "month_col")
    }
  }
  if (!perfect_assignment) {
    required_cols <- c(required_cols, "success_date_col", "assignment_date_col")
  }
  req_reasons <- all_reasons[required_cols]
  required_types <- required_types[required_cols]

  # Check for missing required columns
  purrr::pwalk(
    list(required_cols, req_reasons, required_types),
    ~ {
      if (!..1 %in% names(data_cols)) {
        rlang::abort(c(
          sprintf("Required column `%s` is not declared in `data_cols`.", ..1),
          "x" = sprintf("reason: %s", ..2)
        ))
      }
      provided_col <- data_cols[[..1]]$name
      if (!provided_col %in% names(data)) {
        rlang::abort(c(
          sprintf("Required column `%s` is not found in provided `data`.", ..1),
          "x" = sprintf("reason: %s", ..2),
          "x" = sprintf("Your column: %s", provided_col)
        ))
      }
      data_type <- class(data[[data_cols[[..1]]$name]])
      if (
        !any(vapply(
          ..3$tests,
          \(x) {
            `x`(data[[data_cols[[..1]]$name]])
          },
          FUN.VALUE = logical(1)
        ))
      ) {
        rlang::abort(c(
          sprintf("Required column `%s` is the wrong data type.", ..1),
          "x" = sprintf("Your type: %s", paste(data_type, collapse = ", ")),
          "i" = sprintf(
            "Permissible types: %s",
            paste(..3$classes, collapse = ", ")
          )
        ))
      }
    }
  )

  # Now handle non-required columns that are present but unnecessary
  if (verbose) {
    non_required_cols <- setdiff(all_cols, required_cols)
    non_req_reasons <- list(
      date_col = "assignment_method is not 'date'",
      month_col = "time_unit is not 'month'",
      success_date_col = "perfect_assignment is TRUE",
      assignment_date_col = "perfect_assignment is TRUE"
    )
    non_req_reasons <- non_req_reasons[non_required_cols]

    purrr::walk2(
      non_req_reasons,
      non_req_reasons,
      ~ {
        if (.x %in% names(data_cols)) {
          rlang::warn(c(
            "i" = sprintf(
              "`%s` is not required because %s. It will be ignored.",
              .x,
              .y
            )
          ))
        }
      }
    )
  }
}

#------------------------------------------------------------------------------
#' @title Checking if Inputs are proper Logical Values (TRUE and FALSE)
#' @name check_logical
#' @returns Nothing; Throws an error if any input is not TRUE or FALSE
#' @description Helper to [validate_inputs()]. This function accepts the user's
#' settings for logical values in the Multi-Arm-Bandit trial, and checks whether they are valid.
#' @param ... Arguments to check
#' @keywords internal
check_logical <- function(...) {
  args <- rlang::dots_list(..., .named = TRUE)
  purrr::iwalk(
    args,
    ~ {
      if (!is.logical(.x) || length(.x) != 1 || is.na(.x)) {
        rlang::abort(
          c(
            sprintf("`%s` must be a logical (TRUE or FALSE)", .y),
            "x" = paste0("You Passed: ", .x)
          )
        )
      }
    }
  )
}
#--------------------------------------------------------------------------------
#' @title Checking if inputs are proportions
#' @name check_prop
#' @returns Throws an error if any input is not a valid proportion between 0 and 1
#' @description Helper to [validate_inputs()]. This function accepts the user's
#' settings for proportion arguments and checks if they are valid proportions between 0 and 1
#' @inheritParams check_logical
#' @keywords internal
check_prop <- function(...) {
  args <- rlang::dots_list(..., .named = TRUE)

  purrr::iwalk(
    args,
    ~ {
      if (is.null(.x) || !is.numeric(.x) || .x < 0 || .x > 1) {
        rlang::abort(c(
          sprintf("`%s` must be a non-null double between 0 and 1.", .y),
          "x" = paste0("You passed: ", .x)
        ))
      }
    }
  )
  if (args$control_augment > 0 && args$random_assign_prop > 0) {
    rlang::warn(c(
      "It is not recommended to use control augmentation with hybrid assignment;
                control augmentation only affects bandit assignments."
    ))
  }
}
#-------------------------------------------------------------------------------
#' @title Checking if inputs are positive integers or a valid string
#' @name check_posint
#' @returns Throws an error if any input is not a positive whole number or
#' a valid string.
#' @description Helper to [validate_inputs()]. This function accepts the user's
#' settings for integer arguments and checks if they are valid positive
#' integers or are a one of the valid strings for the argument.
#' @inheritParams check_logical
#' @keywords internal
check_posint <- function(...) {
  args <- rlang::dots_list(..., .named = TRUE)

  valid_strings <- list(
    ndraws = NULL,
    prior_periods = c("all")
  )

  for (name in names(args)) {
    val <- args[[name]]
    valid_string <- valid_strings[[name]]

    if (!is.null(valid_string) && val %in% valid_string) {
      next
    }
    if (is.character(val)) {
      rlang::abort(c(
        sprintf(
          "`%s` must be a positive integer or one of: '%s'",
          name,
          paste(valid_string, collapse = "', '")
        ),
        "x" = paste0("You passed: ", val)
      ))
    }
    if (!posint(val)) {
      rlang::abort(c(
        sprintf("`%s` must be a positive integer", name),
        "x" = paste0("You passed: ", val)
      ))
    }
  }
}
posint <- function(x) {
  return(is.numeric(x) && x > 0 && x %% 1 == 0)
}
#--------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#' @title Checking for valid input data
#' @name check_data
#' @returns Throws an error if the data does not meet the specifications
#' of the trial based on user settings.
#' @description Helper to [validate_inputs()]. This function accepts the data and checks
#' whether it has Unique ID's whether the period length is valid.
#' @inheritParams single_mab_simulation
#' @keywords internal
check_data <- function(
  data,
  data_cols,
  assignment_method,
  period_length,
  time_unit,
  perfect_assignment
) {
  unique_ids <- length(unique(data[[data_cols$id$name]]))
  if (unique_ids != nrow(data)) {
    rlang::abort(paste(
      data_cols$id$name,
      "is not a unique identifier; a unique ID for each observation is required."
    ))
  }

  if (assignment_method == "batch" && period_length > nrow(data)) {
    rlang::abort(c(
      "`period_length` cannot be larger than data size",
      "x" = sprintf(
        "You data has %d rows, and your batch size is %d rows",
        nrow(data),
        period_length
      )
    ))
  }
  if (assignment_method == "date") {
    unit <- switch(
      time_unit,
      "day" = lubridate::days(1),
      "month" = months(1),
      "week" = lubridate::weeks(1)
    )

    data_interval <- lubridate::interval(
      min(data[[data_cols$date_col$name]]),
      max(data[[data_cols$date_col$name]])
    ) /
      unit
    data_interval <- round(data_interval, 0)

    if (period_length > data_interval) {
      rlang::abort(c(
        "`period_length` cannot be larger the date range of your data",
        "x" = sprintf(
          "Your period length is %d %ss but your data only covers %d %ss",
          period_length,
          tolower(time_unit),
          data_interval,
          tolower(time_unit)
        )
      ))
    }
  }
}
# ----------------------------------------------------------------------------
#' @title Checking for valid assignment methods
#' @name check_assign_method
#' @returns Throws an error if the user is missing necessary arguments to
#' assign treatments or passes invalid ones.
#' @description Helper to [validate_inputs()]. This function accepts arguments relating
#' to how treatment waves are assigned, and checks if they are valid, and if all
#' supporting arguments are passed as necessary.
#' @inheritParams single_mab_simulation
#' @keywords internal
check_assign_method <- function(
  assignment_method,
  time_unit,
  verbose,
  period_length
) {
  if (assignment_method == "date") {
    if (
      is.null(time_unit) || length(time_unit) != 1 || isTRUE(is.na(time_unit))
    ) {
      rlang::abort(
        "`time_unit` must be provided when assignment method is `date`."
      )
    }
    if (!time_unit %in% c("day", "week", "month")) {
      rlang::abort(c(
        "Invalid Time Unit",
        "x" = paste0("you passed: ", time_unit),
        "i" = "valid units are `day`, `month`, `week`"
      ))
    }
  }
  if (assignment_method %in% c("batch", "date")) {
    if (is.null(period_length)) {
      rlang::abort(c(
        "`period_length`, must be provided when date or batch assignment is used."
      ))
    }
    if (!posint(period_length)) {
      rlang::abort(c(
        "`period_length` must be a positive integer.",
        "x" = paste0("You passed: ", period_length)
      ))
    }
  }

  if (
    verbose &&
      !assignment_method %in% c("batch", "date") &&
      !is.null(period_length)
  ) {
    rlang::warn(c(
      "i" = "`time_unit` is not required when assignment method is not `date`. It will be ignored"
    ))
  }
}
