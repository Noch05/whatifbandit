#' @title Validates Inputs For [mab_from_rct()]
#' @name check_rct_args
#' @description This function checks to ensure that all required arguments
#' have been properly passed to [mab_from_rct()] before continuing with the simulation. When
#' errors are thrown, user-friendly messages are provided to indicate which argument
#' was misspecified. Additionally, when `verbose = TRUE`, additional warning
#' messages may be shown if unnecessary arguments are passed.
#' @inheritParams mab_from_rct
#' @inheritParams prep_rct_data
#' @returns Throws an error if an argument is missing or misspecified.
#' @keywords internal
#' @family checks
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
  col_names,
  verbose,
  ndraws,
  r,
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
    check_string
  )

  check_logical(
    verbose,
    whole_experiment,
    delayed_feedback,
    keep_data
  )

  check_cols(
    data = data,
    period_method = period_method,
    time_unit = time_unit,
    delayed_feedback = delayed_feedback,
    col_names = col_names,
    verbose = verbose,
    blocking = blocking
  )

  check_period_method(
    period_method = period_method,
    time_unit = time_unit,
    verbose = verbose,
    period_length = period_length
  )

  check_prop(control_augment, random_assign_prop, discount_rate)
  check_posint(r, ndraws, prior_periods)

  check_data(
    data = data,
    col_names = col_names,
    period_method = period_method,
    period_length = period_length,
    time_unit = time_unit,
    delayed_feedback
  )
}
#---------------------------------------------------------------------------------------
#' @describeIn check_rct_args Helper to [check_rct_args()]. This function accepts the user's
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
  col_names,
  data,
  verbose,
  blocking
) {
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
    ),
    cluster_col = list(
      classes = data_types[c(1, 3, 4, 5, 6)],
      tests = test_funcs[c(1, 3, 4, 5, 6)]
    )
  )

  required_cols <- c("success_col", "condition_col")

  if (period_method == "date") {
    required_cols <- c(required_cols, "date_col")
    if (time_unit == "month" && !is.null(col_names[["month_col"]])) {
      required_cols <- c(required_cols, "month_col")
    }
  }
  if (delayed_feedback) {
    required_cols <- c(required_cols, "success_date_col", "assignment_date_col")
  }
  if (!is.null(col_names[["cluster_col"]])) {
    required_cols <- c(required_cols, "cluster_col")
  }
  req_reasons <- all_reasons[required_cols]
  required_types <- required_types[required_cols]

  purrr::pwalk(
    list(required_cols, req_reasons, required_types),
    ~ {
      if (!..1 %in% names(col_names)) {
        rlang::abort(c(
          sprintf("Required column `%s` is not declared in `col_names`.", ..1),
          "x" = sprintf("reason: %s", ..2)
        ))
      }
      provided_col <- col_names[[..1]]
      if (!provided_col %in% names(data)) {
        rlang::abort(c(
          sprintf("Required column `%s` is not found in provided `data`.", ..1),
          "x" = sprintf("reason: %s", ..2),
          "x" = sprintf("Your column: %s", provided_col)
        ))
      }
      data_type <- class(data[[col_names[[..1]]]])
      if (
        !any(vapply(
          ..3[["tests"]],
          \(fn) fn(data[[col_names[[..1]]]]),
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
    purrr::walk(col_names[["block_cols"]], \(col) {
      if (!col %in% names(data)) {
        rlang::abort(sprintf(
          "`%s is not in the data, but was chosen as a block.",
          col
        ))
      }
    })
  }

  if (verbose) {
    non_required_cols <- setdiff(all_cols, required_cols)
    non_req_reasons <- list(
      date_col = "period_method is not 'date'",
      month_col = "time_unit is not 'month'",
      success_date_col = "delayed_feedback is FALSE",
      assignment_date_col = "delayed_feedback is FALSE"
    )
    non_req_reasons <- non_req_reasons[non_required_cols]

    purrr::iwalk(non_req_reasons, \(reason, col_name) {
      if (col_name %in% names(col_names)) {
        rlang::warn(c(
          "i" = sprintf(
            "`%s` is not required because %s. It will be ignored.",
            col_name,
            reason
          )
        ))
      }
    })
  }
}

#' Argument Check Helper Functions
#' @name check_helpers
#' @family checks
#' @param ... Arguments to check
#' @description
#' This set of functions is common across the main argument checkers, and they each
#' check a clear condition on a set of arguments, such ensuring the proper data type.
NULL


#------------------------------------------------------------------------------
#' @describeIn check_helpers Checks for valid logical arguments
#' @returns Throws an error if any input is not TRUE or FALSE
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
#' @describeIn check_helpers This function accepts the user's
#' settings for proportion arguments and checks if they are valid proportions between 0 and 1.
#' @returns Throws an error if any input is not a valid proportion between 0 and 1.
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
#' @describeIn check_helpers This function accepts the user's
#' settings for positive integer arguments and checks if they are valid positive integers.
#' @returns Throws an error if any input is not a valid positive integer.
#' @keywords internal
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
#' @describeIn check_rct_args Throws an error if the provided dataset does not meet the specifications
#' of the trial based on user settings.
#' @returns Nothing; Throws an error if the provided dataset does not meet the specifications
#' of the trial based on user settings.
#' @inheritParams mab_from_rct
#' @inheritParams prep_rct_data
#' @keywords internal
check_data <- function(
  data,
  col_names,
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
      min(data[[col_names[["date_col"]]]]),
      max(data[[col_names[["date_col"]]]])
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
#' @describeIn check_rct_args Helper to [check_rct_args()]. This function accepts arguments relating
#' to how treatment waves are assigned, and checks if they are valid, and if all
#' supporting arguments are passed as necessary.
#' @returns Throws an error if the user is missing necessary arguments to
#' assign treatments or passes invalid ones.
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
#' @description This function checks to ensure that all required arguments
#' have been properly passed to [simulate_mab()] before continuing with the simulation. When
#' errors are thrown, user-friendly messages are provided to indicate which argument
#' was misspecified. Additionally, when `verbose = TRUE`, additional warning
#' messages may be shown if unnecessary arguments are passed.
#' @name check_mab_sim
#' @inheritParams simulate_mab
#' @returns Nothing; Throws an error if all checks are not met.
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
#' @describeIn check_helpers Checks if specified numeric vectors each sum to 1.
#' @returns Nothing; Throws an error if a numeric vector does not sum to 1.
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

#' @describeIn check_helpers Checks specific string arguments against provided valid arguments.
#' @param arg Argument to check.
#' @param valid vector of valid arguments.
#' @param name name of the argument.
#' @returns Nothing; Throws an error of the string argument is invalid.
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
#' @describeIn check_helpers Checks if provided objects have `names` attribute.
#' @returns Nothing; Throws an if an argument does not have `names` attribute.
check_names <- function(...) {
  args <- rlang::dots_list(..., .named = TRUE)
  purrr::iwalk(args, \(arg, name) {
    if (is.null(names(arg))) {
      rlang::abort(c(sprintf("%s must have the `names` attribute", name)))
    }
  })
}

#' @describeIn check_mab_sim Checks if `colnames(p)` matches provided labels
#' @inheritParams simulate_mab
#' @param expected Expected set of group labels.
#' @returns Nothing; Throws an error if `colnames(p)` doesn't match provided labels.

check_p_colnames <- function(p, expected) {
  if (!setequal(colnames(p), expected)) {
    rlang::abort(c(
      "`colnames(p)` must match group labels.",
      "x" = sprintf("Expected: %s", paste(expected, collapse = ", ")),
      "x" = sprintf("Got: %s", paste(colnames(p), collapse = ", "))
    ))
  }
}

#' Checking Clusters Do Not Persist Across Periods
#' @name cluster_check
#' @inheritParams prep_rct_data
#' @inheritParams run_mab
#' @inheritParams estimate_aipw
#' @description
#' Checks to ensure that each cluster only exists within a single simulation period, because
#' if this is the case a true clustered design is no longer specified. See details.
#' @returns Nothing; Throws an error if any clusters persist across multiple periods.
#' @details
#' The assignment algorithm in [mab_loop()] assumes that clusters do not persist across periods. For a
#' true clustered design, if a cluster persisted across periods, all observations within
#' it would have to be assigned to the same treatment as in the previous period. In an adaptive
#' experiment this results in no adaptation, thus this is not implemented
#' into the algorithm. Instead, the assumption is verified here.
cluster_check <- function(
  data,
  cluster_col
) {
  UseMethod("cluster_col", data)
}

#' @method cluster_check data.frame
#' @rdname cluster_check
cluster_check.data.frame <- function(data, cluster_col) {
  cluster_check <- data |>
    dplyr::group_by(.data[[cluster_col]]) |>
    dplyr::summarize(n_periods = dplyr::n_distinct(period_number)) |>
    dplyr::filter(n_periods > 1)

  if (nrow(cluster_check) > 0) {
    rlang::abort(
      c(
        "Clusters must only appear in a single period.",
        "x" = paste(
          "These clusters persist across multiple periods:",
          paste(cluster_check[[cluster_col]], collapse = ", ")
        )
      )
    )
  }
}

#' @method cluster_check data.table
#' @rdname cluster_check
cluster_check.data.table <- function(data, cluster_col) {
  cluster_check <- data[,
    .(n_periods = data.table::uniqueN(period_number)),
    by = cluster_col
  ][n_periods > 1]
  if (nrow(cluster_check) > 0) {
    rlang::abort(
      c(
        "Clusters must only appear in a single period.",
        "x" = paste(
          "These clusters persist across multiple periods:",
          paste(cluster_check[[cluster_col]], collapse = ", ")
        )
      )
    )
  }
}
