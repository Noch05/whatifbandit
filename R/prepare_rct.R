#' @name prep_rct_data
#' @inheritParams mab_from_rct.bernoulli()
#' @title Pre-Simulation Setup to Simulate a Multi Arm Bandit Trial From an RCT

#' @description Common function for all the actions that need to take place before
#' running the Multi-Arm-Bandit re-simulation. Intakes the data and column names to
#' check for valid arguments, format and create new columns as needed, and pre-compute
#' key values to avoid doing so within the simulation loop.
#' @param blocking Logical; Whether or not treatment blocking is occuring
#' @param clustering Logical; Whether or not treatment clustering is occuring
#'
#' @returns Named list containing:
#' \itemize{
#' \item `data_cols`: List of necessary columns in `data` as strings and symbols.
#' \item `data`: Prepared `data.frame` or `data.table` containing all the necessary columns to
#' conduct the adaptive trial simulation.
#' columns required for [mab_simulation()].
#' \item `imputation_information`: List containing necessary information
#' for outcome and date imputation for [mab_simulation()].
#' }
#' @details
#'  If a `data.table` is passed it is copied to avoid modifying the
#' original dataset in the users environment.

#'
#' @keywords internal

prep_rct_data <- function(
  data,
  period_method,
  algorithm,
  control_condition,
  prior_periods,
  delayed_feedback,
  whole_experiment,
  data_cols,
  control_augment,
  time_unit,
  period_length,
  verbose,
  ndraws,
  random_assign_prop,
  check_args,
  r,
  seeds,
  keep_data,
  impute_cluster,
  blocking,
  clustering
) {
  if (base::is.null(data) || !base::is.data.frame(data)) {
    rlang::abort("Input 'data' must be a non-null data.frame.")
  }
  if (data.table::is.data.table(data)) {
    data <- data.table::copy(data)
  }
  data_cols <- base::lapply(data_cols, \(col) {
    rlang_func <- if (base::length(col) > 1) rlang::syms else rlang::sym
    base::list(name = col, sym = rlang_func(x))
  }) |>
    stats::setNames(base::names(data_cols))

  char_args <- base::lapply(
    base::list(
      assignment_method = assignment_method,
      algorithm = algorithm,
      time_unit = time_unit,
    ),
    \(arg) {
      if (base::is.chararcter(arg)) base::tolower(arg) else arg
    }
  )
  # Input Validation
  if (check_args) {
    validate_inputs(
      data = data,
      time_unit = char_args$time_unit,
      delayed_feedback = delayed_feedback,
      algorithm = char_args$algorithm,
      period_length = period_length,
      whole_experiment = whole_experiment,
      prior_periods = prior_periods,
      data_cols = data_cols,
      period_method = char_args$period_method,
      verbose = verbose,
      control_augment = control_augment,
      ndraws = ndraws,
      random_assign_prop = random_assign_prop,
      r = r,
      keep_data = keep_data,
      seeds = seeds,
      blocking = blocking,
      clustering = clustering,
      impute_cluster = impute_cluster
    )
  }
  conditions <- create_conditions(
    control_condition = control_condition,
    data = data,
    condition_col = data_cols$condition_col,
    control_augment = control_augment
  )

  # Preparing Data to be simulated
  verbose_log(verbose, "Preparing Data")

  data <- create_cutoff(
    data = data,
    data_cols = data_cols,
    period_length = period_length,
    period_method = char_args$period_method,
    time_unit = char_args$time_unit
  ) |>
    create_new_cols(
      data_cols = data_cols,
      delayed_feedback = delayed_feedback,
      blocking = blocking,
      impute_cluster = impute_cluster
    )
  # Pre-computing Important values to be accessed for the simulation
  verbose_log(verbose, "Precomputing")

  imputation_information <- imputation_precompute(
    data = data,
    whole_experiment = whole_experiment,
    data_cols = data_cols,
    delayed_feedback = delayed_feedback
  )

  return(list(
    data_cols = data_cols,
    data = data,
    imputation_information = imputation_information,
    char_args = char_args,
    conditions = conditions
  ))
}
#---------------------------------------------------------------------------------
#' @title Creating proper conditions vector
#' @name create_conditions
#' @returns Character vector of unique treatment conditions. Throws error if an invalid specification
#' is used.
#' @description This function creates a character vector of treatment conditions
#' using the conditions column in the provided data, and if `control_augment` is greater
#' than 0, it also labels the control condition. Throws an error of `control_condition` is not
#' present.
#' @inheritParams single_mab_simulation
#' @inheritParams cols
#' @keywords internal
create_conditions <- function(
  control_condition,
  data,
  condition_col,
  control_augment
) {
  conditions <- base::sort(base::as.character(base::unique(data[[
    condition_col$name
  ]])))
  if (control_augment > 0) {
    if (length(control_condition) != 1) {
      rlang::abort(c(
        "`control_condition` must have a length of 1",
        "x" = sprintf(
          "You passed a vector of length: %d",
          length(control_condition)
        )
      ))
    }
    if (
      is.null(control_condition) |
        is.na(control_condition) |
        !as.character(control_condition) %in% conditions
    ) {
      rlang::abort(c(
        "`control_condition` is not present in the conditions column",
        "x" = sprintf(
          "Potential Conditions: %s",
          paste0(conditions, collapse = ", ")
        ),
        "x" = paste0("You Passed: ", base::deparse(control_condition))
      ))
    }

    names(conditions) <- base::ifelse(
      conditions == as.character(control_condition),
      "control",
      "treatment"
    )
  }
  return(conditions)
}
#' Create Treatment Wave Cutoffs
#' @name create_cutoff
#' @description Used to assign each observation a new treatment assignment period, based
#' on user-supplied specifications, and user supplied data from
#' `date_col` and `month_col` in `data_cols`, and the `period_length`. Creates a new
#' column indicating with period each observation belongs to.
#'
#' @inheritParams mab_from_rct.bernoulli
#' @details
#' The assignment periods do not strictly have to line up with the original experiment, it
#' is up to the researcher to test the possible options.
#'
#' Month based assignment can be specified either using the months inside the `month_col` or `date_col`,
#' if `month_col` is passed into the function it will be used.
#'
#' @returns Updated `tibble`/`data.table` with the new `period_number` column. `period_number` is an integer
#' representing an observation's new assignment period.
#' @keywords internal
#------------------------------------------------------------------------------------------
create_cutoff <- function(
  data,
  data_cols,
  period_length = NULL,
  period_method,
  time_unit
) {
  data <- switch(
    period_method,
    "individual" = create_cutoff.individual(data = data),
    "batch" = create_cutoff.batch(data = data, period_length = period_length),
    "date" = create_cutoff.date(
      data = data,
      period_length = period_length,
      date_col = data_cols$date_col,
      month_col = data_cols$month_col,
      time_unit = time_unit
    ),
    rlang::abort(
      "Invalid Assignment Method: valid methods are `individual`, `batch`, `date`"
    )
  )
  return(invisible(data))
}
#------------------------------------------------------------------------------------------
#' @method create_cutoff date
#' @title [create_cutoff()] Date Based Periods
#' @inheritParams mab_from_rct.bernoulli
#' @noRd
create_cutoff.date <- function(
  data,
  time_unit,
  date_col,
  month_col,
  period_length
) {
  time_length <- switch(
    time_unit,
    "day" = lubridate::days(1),
    "week" = lubridate::weeks(1),
    "month" = base::months(1)
  )
  start_date <- base::min(data[[date_col$name]])

  if (data.table::is.data.table(data)) {
    if (time_unit == "month" && !is.null(month_col)) {
      first_month <- data[
        order(base::get(date_col$name)),
        base::get(month_col$name)
      ][1]

      start_month <- lubridate::ymd(base::paste0(
        lubridate::year(start_date),
        "-",
        first_month,
        "-01"
      ))

      data[,
        month_date := lubridate::ymd(
          base::paste0(
            lubridate::year(base::get(date_col$name)),
            "-",
            base::get(month_col$name),
            "-01"
          )
        )
      ]
      data[,
        period_number := base::floor(
          lubridate::interval(start_month, month_date) /
            base::months(1) /
            period_length
        ) +
          1
      ]
      data[, month_date := NULL]

      data.table::setkey(data, period_number)
      data.table::setorderv(data, cols = c(date_col$name, "period_number"))
    } else {
      data[,
        period_number := base::floor(
          lubridate::interval(start_date, base::get(date_col$name)) /
            time_length /
            period_length
        ) +
          1
      ]
      data.table::setkey(data, period_number)
      data.table::setorderv(data, cols = c(date_col$name, "period_number"))
    }
  } else {
    if (time_unit == "month" && !is.null(month_col)) {
      first_month <- data |>
        dplyr::slice_min(order_by = !!date_col$sym, n = 1, with_ties = FALSE) |>
        dplyr::pull(!!month_col$sym)

      start_month <- lubridate::ymd(
        paste0(lubridate::year(start_date), "-", first_month, "-01")
      )
      data <- data |>
        dplyr::mutate(
          month_date = lubridate::ymd(paste0(
            lubridate::year(!!date_col$sym),
            "-",
            !!month_col$sym,
            "-01"
          )),
          period_number = base::floor(
            lubridate::interval(start_month, month_date) /
              base::months(1) /
              period_length
          )
        ) |>
        dplyr::select(-month_date) |>
        dplyr::arrange(!!date_col$sym, period_number)
    } else {
      data <- data |>
        dplyr::mutate(
          period_number = base::floor(
            lubridate::interval(start_date, !!date_col$sym) /
              time_length /
              period_length
          ) +
            1
        ) |>
        dplyr::arrange(!!date_col$sym, period_number)
    }
  }
  return(data)
}

#--------------------------------------------------------------------------

#' @method create_cutoff individual
#' @title [create_cutoff()] Individual Periods
#' @inheritParams mab_from_rct.bernoulli
#' @noRd
#'
create_cutoff.individual <- function(data) {
  if (data.table::is.data.table(data)) {
    data[, period_number := .I]
    data.table::setkey(data, period_number)
    data.table::setorder(data, period_number)
    return(invisible(data))
  } else {
    data <- data |>
      dplyr::mutate(period_number = dplyr::row_number()) |>
      dplyr::arrange(period_number)
    return(data)
  }
}
#----------------------------------------------------------------------------------
#' @method create_cutoff batch
#' @title [create_cutoff()] Batch Based Periods
#' @inheritParams mab_from_rct.bernoulli
#' @noRd
#'
create_cutoff.batch <- function(data, period_length) {
  if (data.table::is.data.table(data)) {
    data[, period_number := base::ceiling((.I / period_length))]
    data.table::setkey(data, period_number)
    data.table::setorder(data, period_number)
    return(invisible(data))
  } else {
    data <- data |>
      dplyr::mutate(
        period_number = base::ceiling(dplyr::row_number() / period_length)
      ) |>
      dplyr::arrange(period_number)
    return(data)
  }
}
#------------------------------------------------------------------------------------
#' @title Create Necessary Columns for Multi-Arm Bandit Trial
#' @name create_new_cols
#' @description Initializes partially empty columns in `data` to initialize them for the simulation.
#' These are initialized as `NA` except for observations with `period_number` = 1, whose values are copied
#' from the provided columns, and used as the starting point for the simulation.
#'
#' @inheritParams mab_from_rct.bernoulli
#' @inheritParams prep_rct_data
#'
#'
#' @returns Updated `data.frame`/`data.table` with 6 new columns:
#' \itemize{
#' \item `mab_success`: New variable to hold new success from Multi-arm bandit procedure, NA until assigned.
#' \item `mab_condition`: New variable to hold new treatment condition from Multi-arm bandit procedure, NA until assigned.
#' \item `impute_req`: Binary indicator for imputation requirement, NA until assigned.
#' \item `new_success_date`: New variable to hold the new success date under Multi-arm bandit procedure, NA until assigned.
#' \item `block`: New variable indicating the variables to block by for assignment.
#' \item `treatment_block`: New variable combining block with original treatment condition.
#' }
#'
#' @keywords internal
create_new_cols <- function(
  data,
  data_cols,
  block_cols,
  blocking,
  delayed_feedback,
  impute_cluster
) {
  base::UseMethod("create_new_cols", data)
}
# --------------------------------------------------

#' @title [create_new_cols()] for `data.frame`s and `tibble`s
#' @method create_new_cols `data.frame`
#' @inheritParams create_new_cols
#' @noRd

create_new_cols.data.frame <- function(
  data,
  data_cols,
  block_cols,
  blocking,
  delayed_feedback,
  impute_cluster
) {
  data <- data |>
    dplyr::mutate(
      period_number = base::match(
        period_number,
        base::sort(base::unique(period_number))
      ),
      mab_success = dplyr::if_else(
        period_number == 1,
        !!data_cols$success_col$sym,
        NA
      ),
      mab_condition = dplyr::if_else(
        period_number == 1,
        base::as.character(!!data_cols$condition_col$sym),
        NA
      ),
      impute_req = dplyr::if_else(period_number == 1, 0, NA),
      impute_group = NA_character_,
      assignment_type = dplyr::if_else(
        period_number == 1,
        "initial",
        NA_character_
      )
    )

  if (!delayed_feedback) {
    data <- data |>
      dplyr::mutate(
        new_success_date = dplyr::if_else(
          period_number == 1,
          !!data_cols$success_date_col$sym,
          NA
        )
      )
  }

  if (blocking) {
    data <- data |>
      dplyr::mutate(
        block = do.call(paste, c(data[, data_cols$block_cols$name], sep = "_"))
      )
    treatment_group_cols <- c(
      data_cols$condition_col$name,
      "block",
      if (impute_cluster) data_cols$cluster_col$name
    )
  } else {
    treatment_group_cols <- c(
      data_cols$condition_col$name,
      if (impute_cluster) data_cols$cluster_col$name
    )
  }
  data <- data |>
    dplyr::mutate(
      treatment_group = do.call(
        paste,
        c(data[, treatment_group_cols], sep = "_")
      )
    )
  return(data)
}
#---------------------------------------------------------------------------------
#' @title [create_new_cols()] for `data.table`s
#' @method create_new_cols `data.table`
#' @inheritParams create_new_cols
#' @noRd

create_new_cols.data.table <- function(
  data,
  data_cols,
  blocking,
  block_cols,
  delayed_feedback
) {
  data[,
    period_number := base::match(
      period_number,
      base::sort(base::unique(period_number))
    )
  ][
    period_number == 1,
    `:=`(
      mab_success = base::get(data_cols$success_col$name),
      mab_condition = base::as.character(base::get(
        data_cols$condition_col$name
      )),
      impute_req = 0,
      impute_block = NA_character_,
      assignment_type = "initial"
    )
  ]
  if (!delayed_feedback) {
    data[
      period_number == 1,
      new_success_date := base::get(data_cols$success_date_col$name)
    ]
  }
  if (blocking) {
    data[,
      block := base::do.call(base::paste, c(.SD, sep = "_")),
      .SDcols = block_cols$name
    ]
    treatment_block_cols <- c(
      data_cols$condition_col$name,
      "block",
      if (impute_cluster) data_cols$cluster_col$name
    )
  } else {
    treatment_block_cols <- c(
      data_cols$condition_col$name,
      if (impute_cluster) data_cols$cluster_col$name
    )
  }
  data[,
    treatment_block := base::do.call(paste, c(.SD, sep = "_")),
    .SDcols = treatment_block_cols
  ]
  return(invisible(data))
}
