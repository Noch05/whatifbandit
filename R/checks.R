#' @title Validates Inputs For [mab_from_rct.bernoulli()]
#' @name check_mab_sim.rct
#' @description This function checks to ensure that all required arguments
#' have been properly passed to the function before continuing with the simulation. When
#' errors are thrown, user-friendly messages are provided to indicate which argument
#' was misspecified. Additionally, when `verbose = TRUE`, additional warning
#' messages may be shown if unnecessary arguments are passed.
#' @inheritParams mab_from_rct.bernoulli
#' @inheritParams prep_rct_data
#' @returns Throws an error if an argument is missing or misspecified.
#' @keywords internal
check_mab_sim.rct <- function(
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
  if (!algorithm %in% c("thompson", "ucb1")) {
    rlang::abort(c(
      "'algorithm' must be 'thompson' or 'ucb1'.",
      "x" = paste0("You passed: ", base::deparse(algorithm))
    ))
  }

  if (!period_method %in% c("individual", "batch", "date")) {
    rlang::abort(c(
      "Invalid `period_method`",
      "x" = paste0("you passed: ", base::deparse(period_method)),
      "i" = "Valid methods are `individual`, `batch`, `date`"
    ))
  }
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
    verbose = verbose
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
          base::typeof(seeds),
          base::length(seeds),
          r
        ),
        "i" = "Reccomended to use `sample.int()` to create proper vector"
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
#' @description Helper to [check_mab_sim.rct()]. This function accepts the user's
#' settings for the Multi-Arm-Bandit trial, and checks whether columns in the data have been properly
#' specified based on these settings.
#' @inheritParams mab_from_rct.bernoulli
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
    "id_col",
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
    id_col = list(
      classes = data_types[c(1, 3, 4, 5)],
      tests = test_funcs[c(1, 3, 4)]
    ),
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
    if (time_unit == "month" && !base::is.null(data_cols$month_col)) {
      required_cols <- c(required_cols, "month_col")
    }
  }
  if (delayed_feedback) {
    required_cols <- c(required_cols, "success_date_col", "assignment_date_col")
  }
  if (!base::is.null(data_cols$cluster_col)) {
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

  if (blocking) {
    purrr::walk(data_cols$block_cols$name, \(col) {
      if (!col %in% base::names(data)) {
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
#' @description Helper to [check_mab_sim.rct()]. This function accepts the user's
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
            "x" = base::paste0("You Passed: ", base::deparse(.x))
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
#' @description Helper to [check_mab_sim.rct()]. This function accepts the user's
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
          "x" = paste0("You passed: ", base::deparse(.x))
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
#' @title Checking If Inputs Are Positive Integers or a Valid String
#' @name check_posint
#' @returns Throws an error if any input is not a positive whole number or
#' a valid string.
#' @description Helper to [check_mab_sim.rct()]. This function accepts the user's
#' settings for integer arguments and checks if they are valid positive
#' integers or are a one of the valid strings for the argument.
#' @inheritParams check_logical
#' @keywords internal
check_posint <- function(...) {
  args <- rlang::dots_list(..., .named = TRUE)
  bad <- !vapply(args, posint, logical(1))
  purrr::walk2(names(args)[bad], args[bad], function(name, val) {
    rlang::abort(c(
      base::sprintf(
        "`%s` must be a positive integer or vector
      of positive integers",
        name
      ),
      "x" = base::paste0(
        "You passed: ",
        base::deparse(val)
      )
    ))
  })
}
posint <- function(x) {
  if (base::is.numeric(x)) {
    return(base::all(x > 0 & x %% 1 == 0))
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
#' @description Helper to [check_mab_sim.rct()]. This function accepts the data and checks
#' whether it has unique ID's whether the period length is valid.
#' @inheritParams mab_from_rct.bernoulli
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
#' @title Checking For Valid Assignment Methods
#' @name check_period_method
#' @returns Throws an error if the user is missing necessary arguments to
#' assign treatments or passes invalid ones.
#' @description Helper to [check_mab_sim.rct()]. This function accepts arguments relating
#' to how treatment waves are assigned, and checks if they are valid, and if all
#' supporting arguments are passed as necessary.
#' @inheritParams mab_from_rct.bernoulli
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
      base::is.null(time_unit) ||
        base::length(time_unit) != 1 ||
        base::isTRUE(base::is.na(time_unit))
    ) {
      rlang::abort(
        "`time_unit` must be provided when assignment method is `date`."
      )
    }
    if (!time_unit %in% c("day", "week", "month")) {
      rlang::abort(c(
        "Invalid Time Unit",
        "x" = paste0("you passed: ", base::deparse(time_unit)),
        "i" = "valid units are `day`, `month`, `week`"
      ))
    }
  }
  if (period_method %in% c("batch", "date")) {
    if (base::is.null(period_length)) {
      rlang::abort(c(
        "`period_length`, must be provided when date or batch based periods are used."
      ))
    }
    if (!posint(period_length)) {
      rlang::abort(c(
        "`period_length` must be a positive integer.",
        "x" = paste0("You passed: ", base::deparse(period_length))
      ))
    }
  }
  if (
    verbose &&
      !period_method %in% c("batch", "date") &&
      !base::is.null(time_unit)
  ) {
    rlang::warn(c(
      "i" = "`time_unit` is not required when assignment method is not `date`. It will be ignored"
    ))
  }
}
#----------------------------------------------------------------------------
#' Perform Validation Checks for [mab_trial_sim.bernoulli()]
#' @description
#' Ensures all arguments to [mab_trial_sim.bernoulli()] are properly
#' provided accordingly.
#' @name check_mab_sim
#' @inheritParams mab_trial_sim.bernoulli
#' @returns Nothing; Throws an error if checks are not met
#' @keywords internal
check_mab_sim <- function(
  n,
  t,
  p,
  algorithm,
  blocks,
  clusters,
  control_augment,
  random_assign_prop,
  dates_of_assignment,
  time_model,
  period_sizes,
  prior_periods,
  discount_rate,
  dt,
  ndraws = 5000,
  r,
  keep_data
) {
  check_logical(dt, keep_data)
  check_posint(n, t, ndraws, r, prior_periods, period_sizes)
  check_prop(control_augment, random_assign_prop, discount_rate)

  if (!base::is.null(blocks) && !base::is.null(clusters)) {
    base::do.call(check_sum1, c(list(blocks), clusters))
  } else if (!base::is.null(clusters)) {
    check_sum1(clusters = clusters)
  } else if (!base::is.null(blocks)) {
    check_sum1(blocks = blocks)
  }

  if (!base::is.null(time_model) && !base::is.function(time_model)) {
    rlang::abort("`time_model` must be a function")
  }

  if (t > n) {
    rlang::abort(
      c("`t` cannot be larger than `n`"),
      "x" = base::sprintf("You Passed: t: %d, n: %d", t, n)
    )
  }
  if (t != base::length(period_sizes)) {
    rlang::abort(
      c(
        "When provided `period_sizes` must be length `t`",
        "x" = base::sprintf("`t`: %d", t),
        "x" = base::paste0(
          "`legnth(period_sizes) = ",
          base::length(period_sizes)
        )
      )
    )
  }

  if (!algorithm %in% c("static", "thompson", "ucb1")) {
    rlang::abort(
      message = c(
        "Invalid Assignment Algorithm",
        "x" = base::sprintf("You passed: %s", algorithm),
        "!" = base::sprintf(
          "Valid Algorithms: %s, %s, %s",
          "thompson",
          "ucb1",
          "static"
        )
      )
    )
  }
  if (base::any(p > 1 | p < 0)) {
    rlang::abort(c(
      "all `p` must be probabilities between 0 and 1",
      "x" = base::paste0("You passed: ", base::paste0(p, collapse = ", "))
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
check_sum1 <- function(...) {
  args <- rlang::dots_list(..., named = TRUE)
  purrr::iwalk(args, \(arg, name) {
    if (sum(arg) != 1) {
      rlang::abort(c(
        base::sprintf("`%s` must sum to 1", name),
        "x" = base::paste0("You passed: ", base::paste0(arg, collapse = ",")),
        "x" = base::paste0("Sum: ", base::sum(arg))
      ))
    }
  })
}
