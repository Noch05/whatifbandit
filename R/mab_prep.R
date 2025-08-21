#' Create Treatment Wave Cutoffs
#' @name create_cutoff
#' @description Used to assign each observation a new treatment assignment period, based
#' on user-supplied specifications, and user supplied data from
#' `date_col` and `month_col` in `data_cols`, and the `period_length`. Creates a new
#' column indicating with period each observation belongs to.
#'
#' @inheritParams single_mab_simulation
#' @inheritParams cols
#' @details
#' The assignment periods do not strictly have to line up with the original experiment, it
#' is up to the researcher to test the possible options.
#'
#' Month based assignment can be specified either using the months inside the `month_col` or `date_col`,
#' if `month_col` is passed into the function it will be used. `month_col` should only be provided separately
#' when the months inside the column are different than what `format(data[[date_col]], "%m")`, `format(data[[data_col]], "%B")`
#' or `lubridate::month(data[[data_col]])` would return.
#'
#' @returns Updated tibble/data.table with the new `period_number` column. `period_number` is an integer
#' representing an observation's new assignment period.
#' @keywords internal
#------------------------------------------------------------------------------------------
create_cutoff <- function(
  data,
  data_cols,
  period_length = NULL,
  assignment_method,
  time_unit
) {
  data <- switch(
    assignment_method,
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
#' @inheritParams create_cutoff
#' @inheritParams cols
#' @noRd
create_cutoff.date <- function(
  data,
  time_unit,
  date_col,
  month_col = NULL,
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
    if (time_unit == "month" && !is.null(data[[month_col$name]])) {
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
    } else {
      data[,
        period_number := base::floor(
          lubridate::interval(start_date, base::get(date_col$name)) /
            time_length /
            period_length
        ) +
          1
      ]
    }
  } else {
    if (time_unit == "month" && !is.null(data[[month_col$name]])) {
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
        dplyr::select(-month_date)
    } else {
      data <- data |>
        dplyr::mutate(
          period_number = base::floor(
            lubridate::interval(start_date, !!date_col$sym) /
              time_length /
              period_length
          ) +
            1
        )
    }
  }
  return(data)
}

#--------------------------------------------------------------------------

#' @method create_cutoff individual
#' @title [create_cutoff()] Individual Periods
#' @inheritParams create_cutoff
#' @noRd
#'
create_cutoff.individual <- function(data) {
  if (data.table::is.data.table(data)) {
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
#' @method create_cutoff batch
#' @title [create_cutoff()] Batch Based Periods
#' @inheritParams create_cutoff
#' @noRd
#'
create_cutoff.batch <- function(data, period_length) {
  if (data.table::is.data.table(data)) {
    data[, period_number := base::ceiling((.I / period_length))]
    data.table::setkey(data, period_number)
    return(invisible(data))
  } else {
    data <- data |>
      dplyr::mutate(
        period_number = base::ceiling(dplyr::row_number() / period_length)
      )
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
#' @inheritParams single_mab_simulation
#'
#'
#' @returns Updated tibble/data.table with 6 new columns:
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
  perfect_assignment
) {
  base::UseMethod("create_new_cols", data)
}
# --------------------------------------------------

#' @title [create_new_cols()] for data.frames and tibbles
#' @method create_new_cols data.frame
#' @inheritParams create_new_cols
#' @noRd

create_new_cols.data.frame <- function(
  data,
  data_cols,
  block_cols,
  blocking,
  perfect_assignment
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
      impute_block = NA_character_,
      assignment_type = dplyr::if_else(
        period_number == 1,
        "initial",
        NA_character_
      )
    )

  if (!perfect_assignment) {
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
    if (base::is.null(block_cols)) {
      rlang::abort("If blocking is TRUE, blocking variables must be specified")
    }

    data <- data |>
      dplyr::mutate(
        block = do.call(paste, c(data[, block_cols$name], sep = "_")),
        treatment_block = do.call(
          paste,
          c(data[, c(data_cols$condition_col$name, block_cols$name)], sep = "_")
        )
      )
  } else {
    data <- data |>
      dplyr::mutate(
        treatment_block = as.character(!!data_cols$condition_col$sym)
      )
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

create_new_cols.data.table <- function(
  data,
  data_cols,
  blocking,
  block_cols,
  perfect_assignment
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
  if (!perfect_assignment) {
    data[
      period_number == 1,
      new_success_date := base::get(data_cols$success_date_col$name)
    ]
  }

  if (blocking) {
    if (base::is.null(block_cols)) {
      rlang::abort("If blocking is TRUE, blocking variables must be specified")
    }

    data[,
      block := base::do.call(base::paste, c(.SD, sep = "_")),
      .SDcols = block_cols$name
    ]
    data[,
      treatment_block := base::do.call(paste, c(.SD, sep = "_")),
      .SDcols = c(data_cols$condition_col$name, block_cols$name)
    ]
  } else {
    data[, treatment_block := as.character(get(data_cols$condition_col$name))]
  }
  data.table::setkeyv(data, cols = c("period_number", data_cols$id$name))
  data.table::setorderv(data, cols = c("period_number", data_cols$id$name))

  return(invisible(data))
}
