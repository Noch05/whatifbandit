#' Create Treatment Wave Cutoffs
#' @name create_cutoff
#' @description Used during [pre_mab_simulation()] to assign each observation a new treatment assignment period, based
#' on user-supplied specifications, and user supplied data from
#' `date_col` and `month_col` in `data_cols`.
#' @inheritParams single_mab_simulation
#' @inheritParams cols
#'
#' @return Updated `data` object with the new `period_number` column. `period_number` is an integer
#' representing an observation's new assignment period.
#' @seealso
#' *[pre_mab_simulation()]
#' @keywords internal
#------------------------------------------------------------------------------------------
create_cutoff <- function(data, data_cols, period_length = NULL,
                          assignment_method, time_unit) {
  data <- switch(assignment_method,
    "Individual" = create_cutoff.Individual(data = data),
    "Batch" = create_cutoff.Batch(data = data, period_length = period_length),
    "Date" = switch(time_unit,
      "Day" = create_cutoff.Day(
        data = data,
        date_col = data_cols$date_col,
        period_length = period_length
      ),
      "Month" = create_cutoff.Month(
        data = data,
        month_col = data_cols$month_col,
        date_col = data_cols$date_col,
        period_length = period_length
      ),
      "Week" = create_cutoff.Week(
        data = data,
        date_col = data_cols$date_col,
        period_length = period_length
      ),
      rlang::abort("Invalid Time Unit: Valid Units are `Week`, `Month`, and `Day`")
    ),
    rlang::abort("Invalid Assignment Method: valid methods are `Individual`, `Batch`, `Date`")
  )
  return(invisible(data))
}
#------------------------------------------------------------------------------------------

#' @method create_cutoff Day
#' @title [create_cutoff()] Day Based Periods
#' @inheritParams create_cutoff
#' @inheritParams cols
#' @noRd
#'
create_cutoff.Day <- function(data, date_col, period_length) {
  start_date <- base::min(data[[date_col$name]])
  if (inherits(data, "data.table")) {
    data[, period_number := base::floor(
      lubridate::interval(start_date, base::get(date_col$name)) / lubridate::days(1) / period_length
    ) + 1]

    return(invisible(data))
  } else {
    data <- data |>
      dplyr::mutate(
        period_number = base::floor(
          lubridate::interval(start_date, !!date_col$sym) / lubridate::days(1) / period_length
        ) + 1
      )
    return(data)
  }
}
#------------------------------------------------------------------
#' @method create_cutoff Week
#' @title [create_cutoff()] Week Based Periods
#' @inheritParams create_cutoff
#' @inheritParams cols
#' @noRd
create_cutoff.Week <- function(data, date_col, period_length) {
  if (inherits(data, "data.table")) {
    start_date <- base::min(data[, get(date_col$name)])

    data[, period_number := base::floor(
      lubridate::interval(start_date, base::get(date_col$name)) / lubridate::weeks(1) / period_length
    ) + 1]
    data.table::setkey(data, period_number)

    return(invisible(data))
  } else {
    start_date <- base::min(dplyr::pull(data, !!date_col$sym), na.rm = TRUE)

    data <- data |>
      dplyr::mutate(
        period_number = base::floor(
          lubridate::interval(start_date, !!date_col$sym) / lubridate::weeks(1) / period_length
        ) + 1
      )
    return(data)
  }
}
#------------------------------------------------------------------

#' #' @method create_cutoff Month
#' @title [create_cutoff()] Month Based Periods
#' @inheritParams create_cutoff
#' @inheritParams cols
#' @noRd
#'
create_cutoff.Month <- function(data, date_col, month_col, period_length) {
  start_date <- base::min(data[[date_col$name]])

  if (inherits(data, "data.table")) {
    first_month <- data[order(base::get(date_col$name)), base::get(month_col$name)][1]

    start_month <- lubridate::ymd(base::paste0(lubridate::year(start_date), "-", first_month, "-01"))

    data[, month_date := lubridate::ymd(
      base::paste0(lubridate::year(base::get(date_col$name)), "-", base::get(month_col$name), "-01")
    )]

    data[, period_number := base::floor(
      lubridate::interval(start_month, month_date) / base::months(1) / period_length
    ) + 1]

    data[, month_date := NULL]

    return(invisible(data))
  } else {
    first_month <- data |>
      dplyr::slice_min(order_by = !!date_col$sym, n = 1, with_ties = FALSE) |>
      dplyr::pull(!!month_col$sym)

    start_month <- lubridate::ymd(
      paste0(lubridate::year(start_date), "-", first_month, "-01")
    )
    data <- data |>
      dplyr::mutate(
        month_date = lubridate::ymd(paste0(
          lubridate::year(!!date_col$sym), "-", !!month_col$sym, "-01"
        )),
        period_number = base::floor(
          lubridate::interval(start_month, month_date) / base::months(1) / period_length
        )
      ) |>
      dplyr::select(-month_date)

    return(data)
  }
}


#' @method create_cutoff Individual
#' @title [create_cutoff()] Individual Periods
#' @inheritParams create_cutoff
#' @noRd
#'
create_cutoff.Individual <- function(data) {
  if (inherits(data, "data.table")) {
    data[, period_number := .I]
    data.table::setkey(data, period_number)
    return(invisible(data))
  } else {
    data <- data |>
      dplyr::mutate(period_number = dplyr::row_number())
    return(data)
  }
}
#----------------------------------------------------------------------------------
#' @method create_cutoff Batch
#' @title [create_cutoff()] Batch Based Periods
#' @inheritParams create_cutoff
#' @noRd
#'
create_cutoff.Batch <- function(data, period_length) {
  if (inherits(data, "data.table")) {
    data[, period_number := base::ceiling((.I / period_length))]
    data.table::setkey(data, period_number)
    return(invisible(data))
  } else {
    data <- data |>
      dplyr::mutate(period_number = base::ceiling(dplyr::row_number() / period_length))
    return(data)
  }
}
#------------------------------------------------------------------------------------
#' @title Create Necessary Columns for Multi-Arm Bandit Trial
#' @name create_new_cols
#' @description Initializes partially empty columns in `data`, to ensure compatibility with, [mab_simulation()].
#' These are initialized as `NA` except for observations with `period_number` = 1, whose are the starting point for
#' the adaptive trial.
#'
#' @inheritParams single_mab_simulation
#'
#'
#' @returns Updated `data` object of same class with 6 new columns:
#' \itemize{
#' \item mab_success: New variable to hold new success from Multi-arm bandit procedure, NA until assigned.
#' \item mab_condition: New variable to hold new treatment condition from Multi-arm bandit procedure, NA until assigned.
#' \item impute_req: Binary indicator for imputation requirement, NA until assigned.
#' \item new_success_date: New variable to new recertification date from Multi-arm bandit procedure, NA until assigned.
#' \item block New variable indicating the variables to block by for assignment.
#' \item treatment_block New variable combining block with original treatment condition.
#' }
#'
#' @seealso
#' *[create_cutoff()]
#' *[pre_mab_simulation()]
#'
#' @keywords internal
create_new_cols <- function(data,
                            data_cols,
                            block_cols,
                            blocking,
                            perfect_assignment) {
  base::UseMethod("create_new_cols", data)
}
# --------------------------------------------------

#' @title [create_new_cols()] for data.frames and tibbles
#' @method create_new_cols data.frame
#' @inheritParams create_new_cols
#' @noRd

create_new_cols.data.frame <- function(data,
                                       data_cols,
                                       block_cols,
                                       blocking,
                                       perfect_assignment) {
  data <- data |>
    dplyr::mutate(
      period_number = base::match(period_number, base::sort(base::unique(period_number))),
      mab_success = dplyr::if_else(period_number == 1, !!data_cols$success_col$sym, NA_real_),
      mab_condition = dplyr::if_else(period_number == 1, !!data_cols$condition_col$sym, NA_character_),
      impute_req = dplyr::if_else(period_number == 1, 0, NA_real_),
      impute_block = NA_character_
    )

  if (!perfect_assignment) {
    data <- data |>
      dplyr::mutate(new_success_date = dplyr::if_else(period_number == 1, !!data_cols$success_date_col$sym, NA))
  }

  if (blocking) {
    if (base::is.null(block_cols)) {
      rlang::abort("If blocking is TRUE, blocking variables must be specified")
    }

    data <- data |>
      dplyr::mutate(
        block = do.call(paste, c(data[, block_cols$name], sep = "_")),
        treatment_block = do.call(paste, c(data[, c(data_cols$condition_col$name, block_cols$name)], sep = "_"))
      )
  } else {
    data <- data |>
      dplyr::mutate(treatment_block = !!data_cols$condition_col$sym)
  }
  data <- data |>
    dplyr::arrange(period_number, !!data_cols$id$sym)

  return(data)
}
#---------------------------------------------------------------------------------
#' @title [create_new_cols()] for Data.tables
#' @method create_new_cols data.table
#' @inheritParams create_new_cols
#' @noRd

create_new_cols.data.table <- function(data,
                                       data_cols,
                                       blocking,
                                       block_cols,
                                       perfect_assignment) {
  data[, period_number := base::match(period_number, base::sort(base::unique(period_number)))][
    period_number == 1, `:=`(
      mab_success = base::get(data_cols$success_col$name),
      mab_condition = base::get(data_cols$condition_col$name),
      impute_req = 0,
      impute_block = NA_character_
    )
  ]
  if (!perfect_assignment) {
    data[period_number == 1, new_success_date := base::get(data_cols$success_date_col$name)]
  }


  if (blocking) {
    if (base::is.null(block_cols)) {
      rlang::abort("If blocking is TRUE, blocking variables must be specified")
    }

    data[, block := base::do.call(base::paste, c(.SD, sep = "_")), .SDcols = block_cols$name]
    data[, treatment_block := base::do.call(paste, c(.SD, sep = "_")),
      .SDcols = c(data_cols$condition_col$name, block_cols$name)
    ]
  } else {
    data[, treatment_block := get(data_cols$condition_col$name)]
  }
  data.table::setkeyv(data, cols = c("period_number", data_cols$id$name))
  data.table::setorderv(data, cols = c("period_number", data_cols$id$name))

  return(invisible(data))
}
