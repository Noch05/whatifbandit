#' @title Validates Inputs For [mab_from_rct()]
#' @name check_rct_args
#' @description This function checks to ensure that all required arguments
#' have been properly passed to the function before continuing with the simulation. When
#' errors are thrown, user-friendly messages are provided to indicate which argument
#' was misspecified. Additionally, when `verbose = TRUE`, additional warning
#' messages may be shown if unnecessary arguments are passed.
#' @inheritParams mab_from_rct
#' @inheritParams prep_rct_data
#' @returns Throws an error if an argument is missing or misspecified.
#' @keywords internal
check_rct_args <- function(
  data,
  algorithm,
  control_augment,
  random_assign_prop,
  period_method,
  time_unit,
  period_length,
  prior_periods,
  discount_rate,
  delayed_feedback,
  whole_experiment,
  data_cols,
  verbose,
  ndraws,
  r,
  seeds,
  keep_data,
  blocking,
  clustering
) {
  purrr::pwalk(
    list(
      c(algorithm, period_method),
      list(
        c("thompson", "ucb1"),
        c("individual", "batch", "date")
      ),
      c("algorithm", "period_method")
    ),
    \(arg, val, name) {
      check_string(arg, val, name)
    }
  )

  # Checking Logical values
  check_logical(
    verbose,
    whole_experiment,
    delayed_feedback,
    keep_data
  )

  # Checking Column Proper Columns are Provided
  check_cols(
    data = data,
    period_method = period_method,
    time_unit = time_unit,
    delayed_feedback = delayed_feedback,
    data_cols = data_cols,
    verbose = verbose,
    blocking = blocking
  )

  # Checking Period Method Arguments
  check_period_method(
    period_method = period_method,
    time_unit = time_unit,
    verbose = verbose,
    period_length = period_length
  )

  # Checking Numeric Arguments

  check_prop(control_augment, random_assign_prop, discount_rate)
  check_posint(r, ndraws, prior_periods)

  if (r > 1) {
    if (!is.integer(seeds) || length(seeds) != r) {
      rlang::abort(c(
        "Argument 'seeds' must be an integer vector of length equal to `r`. Please provide a valid vector.",
        "x" = sprintf(
          "You passed a %s vector of length %d, while `r` is %d.",
          typeof(seeds),
          length(seeds),
          r
        ),
        "i" = "Recommended to use `sample.int()` to create proper vector"
      ))
    }
  }
  # Checking Data Structure
  check_data(
    data = data,
    data_cols = data_cols,
    period_method = period_method,
    period_length = period_length,
    time_unit = time_unit,
    delayed_feedback
  )
}
#---------------------------------------------------------------------------------------
#'
#' @title Checking existence and declaration of columns
#' @name check_cols
#' @description Helper to [check_rct_args()]. This function accepts the user's
#' settings for the Multi-Arm-Bandit trial, and checks whether columns in the data have been properly
#' specified based on these settings.
#' @inheritParams mab_from_rct
#' @inheritParams prep_rct_data
#' @returns Throws an error if columns which are required have not been declared
#' or are not present in the data, or are the wrong primitive data type. Additionally throws warning messages,
#' if unnecessary columns have been provided, only when `verbose = TRUE`.
#' @keywords internal
#'
check_cols <- function(
  period_method,
  time_unit,
  delayed_feedback,
  data_cols,
  data,
  verbose,
  blocking
) {
  # All possible columns
  all_cols <- c(
    "success_col",
    "condition_col",
    "date_col",
    "month_col",
    "success_date_col",
    "assignment_date_col",
    "block_cols",
    "cluster_col"
  )

  # Reason each column might be required
  all_reasons <- list(
    success_col = "it is always required",
    condition_col = "it is always required",
    date_col = "period_method is 'date'",
    month_col = "time_unit is 'month' and you provided a `month_col`",
    success_date_col = "delayed_feedback is TRUE",
    assignment_date_col = "delayed_feedback is TRUE",
    cluster_col = "it is always required when provided in `formula`"
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
  required_cols <- c("success_col", "condition_col")

  if (period_method == "date") {
    required_cols <- c(required_cols, "date_col")
    if (time_unit == "month" && !is.null(data_cols[["month_col"]])) {
      required_cols <- c(required_cols, "month_col")
    }
  }
  if (delayed_feedback) {
    required_cols <- c(required_cols, "success_date_col", "assignment_date_col")
  }
  if (!is.null(data_cols[["cluster_col"]])) {
    required_cols <- c(required_cols, "cluster_cols")
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
      provided_col <- data_cols[[..1]][["name"]]
      if (!provided_col %in% names(data)) {
        rlang::abort(c(
          sprintf("Required column `%s` is not found in provided `data`.", ..1),
          "x" = sprintf("reason: %s", ..2),
          "x" = sprintf("Your column: %s", provided_col)
        ))
      }
      data_type <- class(data[[data_cols[[..1]][["name"]]]])
      if (
        !any(vapply(
          ..3[["tests"]],
          \(x) {
            `x`(data[[data_cols[[..1]][["name"]]]])
          },
          FUN.VALUE = logical(1)
        ))
      ) {
        rlang::abort(c(
          sprintf("Required column `%s` is the wrong data type.", ..1),
          "x" = sprintf("Your type: %s", paste(data_type, collapse = ", ")),
          "i" = sprintf(
            "Permissible types: %s",
            paste(..3[["classes"]], collapse = ", ")
          )
        ))
      }
    }
  )

  if (blocking) {
    purrr::walk(data_cols[["block_cols"]][["name"]], \(col) {
      if (!col %in% names(data)) {
        rlang::abort(sprintf(
          "`%s is not in the data, but was chosen as a block.",
          col
        ))
      }
    })
  }

  # Now handle non-required columns that are present but unnecessary
  if (verbose) {
    non_required_cols <- setdiff(all_cols, required_cols)
    non_req_reasons <- list(
      date_col = "period_method is not 'date'",
      month_col = "time_unit is not 'month'",
      success_date_col = "delayed_feedback is FALSE",
      assignment_date_col = "delayed_feedback is FALSE"
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
#' @title Checking if Inputs are Logical Values (TRUE and FALSE)
#' @name check_logical
#' @returns Throws an error if any input is not TRUE or FALSE
#' @description Helper to [check_rct_args()]. This function accepts the user's
#' settings for logical values in the Multi-Arm-Bandit trial, and checks whether they are valid.
#' @param ... Arguments to check.
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
            "x" = paste0("You Passed: ", deparse(.x))
          )
        )
      }
    }
  )
}
#--------------------------------------------------------------------------------
#' @title Checking for Proportions
#' @name check_prop
#' @returns Throws an error if any input is not a valid proportion between 0 and 1
#' @description Helper to [check_rct_args()]. This function accepts the user's
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
          "x" = paste0("You passed: ", deparse(.x))
        ))
      }
    }
  )
  if (args[["control_augment"]] > 0 && args[["random_assign_prop"]] > 0) {
    rlang::warn(c(
      "It is not recommended to use control augmentation with hybrid assignment;
                control augmentation only affects bandit assignments."
    ))
  }
}
#-------------------------------------------------------------------------------
#' @title Checking If Inputs Are Positive Integers or a Valid String
#' @name check_posint
#' @returns Throws an error if any input is not a positive whole number or
#' a valid string.
#' @description Helper to [check_rct_args()]. This function accepts the user's
#' settings for integer arguments and checks if they are valid positive
#' integers or are a one of the valid strings for the argument.
#' @inheritParams check_logical
#' @keywords internal
check_posint <- function(...) {
  args <- rlang::dots_list(..., .named = TRUE)
  bad <- !vapply(args, \(arg) posint(arg) || is.null(arg), logical(1))
  purrr::walk2(names(args)[bad], args[bad], function(name, val) {
    rlang::abort(c(
      sprintf(
        "`%s` must be a positive integer or vector
      of positive integers",
        name
      ),
      "x" = paste0(
        "You passed: ",
        deparse(val)
      )
    ))
  })
}
posint <- function(x) {
  if (is.numeric(x)) {
    return(all(x > 0 & x %% 1 == 0))
  } else {
    return(FALSE)
  }
}
#--------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#' @title Checking for Valid Input Data
#' @name check_data
#' @returns Throws an error if the data does not meet the specifications
#' of the trial based on user settings.
#' @description Helper to [check_rct_args()]. This function accepts the data and checks
#' whether it has unique ID's whether the period length is valid.
#' @inheritParams mab_from_rct
#' @inheritParams prep_rct_data
#' @keywords internal
check_data <- function(
  data,
  data_cols,
  period_method,
  period_length,
  time_unit,
  delayed_feedback
) {
  if (period_method == "batch" && period_length > nrow(data)) {
    rlang::abort(c(
      "`period_length` cannot be larger than data size",
      "x" = sprintf(
        "You data has %d rows, and your batch size is %d rows",
        nrow(data),
        period_length
      )
    ))
  }
  if (period_method == "date") {
    unit <- switch(
      time_unit,
      "day" = lubridate::days(1),
      "month" = months(1),
      "week" = lubridate::weeks(1)
    )

    data_interval <- lubridate::interval(
      min(data[[data_cols[["date_col"]][["name"]]]]),
      max(data[[data_cols[["date_col"]][["name"]]]])
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
#' @title Checking For Valid Assignment Methods
#' @name check_period_method
#' @returns Throws an error if the user is missing necessary arguments to
#' assign treatments or passes invalid ones.
#' @description Helper to [check_rct_args()]. This function accepts arguments relating
#' to how treatment waves are assigned, and checks if they are valid, and if all
#' supporting arguments are passed as necessary.
#' @inheritParams mab_from_rct
#' @inheritParams prep_rct_data
#' @keywords internal
check_period_method <- function(
  period_method,
  time_unit,
  verbose,
  period_length
) {
  if (period_method == "date") {
    if (
      is.null(time_unit) ||
        length(time_unit) != 1 ||
        isTRUE(is.na(time_unit))
    ) {
      rlang::abort(
        "`time_unit` must be provided when assignment method is `date`."
      )
    }
    check_string(time_unit, c("day", "week", "month"), "Time Unit")
  }
  if (period_method %in% c("batch", "date")) {
    if (is.null(period_length)) {
      rlang::abort(c(
        "`period_length`, must be provided when date or batch based periods are used."
      ))
    }
    if (!posint(period_length)) {
      rlang::abort(c(
        "`period_length` must be a positive integer.",
        "x" = paste0("You passed: ", deparse(period_length))
      ))
    }
  }
  if (
    verbose &&
      !period_method %in% c("batch", "date") &&
      !is.null(time_unit)
  ) {
    rlang::warn(c(
      "i" = "`time_unit` is not required when assignment method is not `date`. It will be ignored"
    ))
  }
}
#----------------------------------------------------------------------------
#' Perform Validation Checks for [simulate_mab()]
#' @description
#' Ensures all arguments to [simulate_mab()] are properly
#' provided accordingly.
#' @name check_mab_sim
#' @inheritParams simulate_mab
#' @returns Nothing; Throws an error if checks are not met
#' @keywords internal
check_mab_sim <- function(
  n,
  t,
  p,
  algorithm,
  blocks = NULL,
  clusters = NULL,
  control_augment,
  random_assign_prop,
  assignment_dates,
  delayed_feedback,
  time_model = NULL,
  period_sizes = NULL,
  prior_periods = NULL,
  discount_rate,
  dt,
  ndraws = 5000,
  r,
  keep_data,
  verbose
) {
  check_logical(dt, keep_data, verbose)
  check_posint(n, t, ndraws, r, prior_periods, period_sizes)
  check_prop(control_augment, random_assign_prop, discount_rate)
  check_string(algorithm, c("static", "thompson", "ucb1"), "algorithm")

  if (t > n) {
    rlang::abort(
      c("`t` cannot be larger than `n`"),
      "x" = sprintf("You Passed: t: %d, n: %d", t, n)
    )
  }

  if (!is.null(period_sizes) && t != length(period_sizes)) {
    rlang::abort(c(
      "When provided `period_sizes` must be length `t`",
      "x" = sprintf("`t`: %d", t),
      "x" = sprintf("`length(period_sizes)` = %d", length(period_sizes))
    ))
  }
  if (!is.null(assignment_dates) && !lubridate::is.Date(assignment_dates)) {
    rlang::abort("`assignment_dates` must be a `Date` vector")
  }

  if (!is.null(time_model) && !is.function(time_model)) {
    rlang::abort("`time_model` must be a function")
  }

  if (delayed_feedback) {
    if (is.null(time_model)) {
      rlang::abort(c(
        "`time_model` must be provided when `delayed_feedback = TRUE`.",
        "x" = "`time_model` is NULL"
      ))
    }
    if (is.null(assignment_dates)) {
      rlang::abort(c(
        "`assignment_dates` must be provided when `delayed_feedback = TRUE`.",
        "x" = "`assignment_dates` is NULL"
      ))
    }
  } else if (!is.null(time_model) && !is.null(assignment_dates)) {
    rlang::warn(c(
      "`time_model` and `assignment_dates` are provided but `delayed_feedback = FALSE`.",
      "i" = "Counterfactual success dates will be simulated but not used for assignment."
    ))
  }

  if (!is.matrix(p) || !is.numeric(p)) {
    rlang::abort("`p` must be a numeric matrix")
  }
  if (is.null(rownames(p))) {
    rlang::abort(c(
      "`p` must have rownames corresponding to treatment conditions.",
      "x" = "`rownames(p)` is NULL"
    ))
  }

  if (any(p > 1 | p < 0)) {
    rlang::abort(c(
      "all `p` must be probabilities between 0 and 1",
      "x" = paste0("You passed: ", paste0(p, collapse = ", "))
    ))
  }

  if (!is.null(blocks) && !is.null(clusters)) {
    do.call(check_sum1, c(list(blocks), clusters))
    do.call(check_names, c(list(blocks), clusters, list(clusters)))
    if (!setequal(names(clusters), names(blocks))) {
      rlang::abort(c(
        "`names(clusters)` must match `names(blocks)` for nested structure.",
        "x" = sprintf(
          "block labels: %s",
          paste(names(blocks), collapse = ", ")
        ),
        "x" = sprintf(
          "cluster labels: %s",
          paste(names(clusters), collapse = ", ")
        )
      ))
    }
    check_p_colnames(p, unlist(lapply(clusters, names)))
  } else if (!is.null(clusters)) {
    check_sum1(clusters = clusters)
    check_names(clusters)
    check_p_colnames(p, names(clusters))
  } else if (!is.null(blocks)) {
    check_sum1(blocks = blocks)
    check_names(blocks)
    check_p_colnames(p, names(blocks))
  } else if (ncol(p) != 1) {
    rlang::abort(c(
      "`p` must have exactly 1 column when no blocks or clusters are provided.",
      "x" = sprintf("`ncol(p)` = %d", ncol(p))
    ))
  }
}
#-------------------------------------------------------------------------------
#' Validates Summing to 1
#' @name check_sum1
#' @description
#' Checks specified numeric vector sums to 1, throws an error if not
#' @param ... Arguments to check.
#' @returns Nothing; Throws an error if the check fails
#' @keywords internal
check_sum1 <- function(...) {
  args <- rlang::dots_list(..., .named = TRUE)
  purrr::iwalk(args, \(arg, name) {
    if (!dplyr::near(sum(arg), 1)) {
      rlang::abort(c(
        sprintf("`%s` must sum to 1", name),
        "x" = paste0("You passed: ", paste0(arg, collapse = ",")),
        "x" = paste0("Sum: ", sum(arg))
      ))
    }
  })
}

#' Validates String Arguments
#' @name check_string
#' @description
#' Checks specific string arguments against provided valid arguments
#' @param arg Argument to check
#' @param valid vector of valid arguments
#' @param name name of the argument
#' @returns Nothing; Throws an error if check fails
check_string <- function(arg, valid, name) {
  if (!arg %in% valid) {
    rlang::abort(
      c(
        sprintf("Invalid `%s`", name),
        "i" = sprintf(
          "Valid Options: %s",
          paste0(valid, collapse = ", ")
        ),
        "x" = sprintf("You Provided: '%s'", arg)
      )
    )
  }
}

#' Validates Names
#' @name check_names
#' @description
#' Checks if provided objects have `names` attribute
#' @param ... objects to check
#' @returns Nothing; Throws an error if check fails
check_names <- function(...) {
  args <- rlang::dots_list(..., .named = TRUE)
  purrr::iwalk(args, \(arg, name) {
    if (is.null(names(arg))) {
      rlang::abort(c(sprintf("%s must have the `names` attribute", name)))
    }
  })
}

#' Validates column names for `p` matrix
#' @description
#' Checks if `colnames(p)` matches provided labels
#' @inheritParams simulate_mab
#' @param expected Expected set of group labels
#' @returns Nothing; Throws an error if condition is not met

check_p_colnames <- function(p, expected) {
  if (!setequal(colnames(p), expected)) {
    rlang::abort(c(
      "`colnames(p)` must match group labels.",
      "x" = sprintf("Expected: %s", paste(expected, collapse = ", ")),
      "x" = sprintf("Got: %s", paste(colnames(p), collapse = ", "))
    ))
  }
}
