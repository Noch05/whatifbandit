#' Create Treatment Wave Cutoffs
#' @name create_cutoff
#' @description Used during [mab_prepare()] to assign treatment periods
#'
#' @inheritParams single_mab_simulation
#'
#' @return data.frame containing the original data with 1 additional column.
#' \item{period_number}{Integer identifying the treatment wave for each observation}
#' @seealso
#' *[mab_prepare()]
#------------------------------------------------------------------------------------------
create_cutoff <- function(data, date_col = NULL, month_col = NULL, period_length = NULL,
                          assignment_method, time_unit) {
  data <- switch(assignment_method,
    "Individual" = create_cutoff.Individual(data = data),
    "Batch" = create_cutoff.Batch(data = data, period_length = period_length),
    "Date" = switch(time_unit,
      "Day" = create_cutoff.Day(
        data = data,
        date_col = {{ date_col }},
        period_length = period_length
      ),
      "Month" = create_cutoff.Month(
        data = data,
        month_col = {{ month_col }},
        date_col = {{ date_col }},
        period_length = period_length
      ),
      "Week" = create_cutoff.Week(
        data = data,
        date_col = {{ date_col }},
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
#'
create_cutoff.Day <- function(data, date_col, period_length) {
  if (inherits(data, "data.table")) {
    date_col <- rlang::as_name(rlang::enquo(date_col))
    start_date <- base::min(data[, get(date_col)])

    data[, period_number := base::floor(
      lubridate::interval(start_date, base::get(date_col)) / lubridate::days(1) / period_length
    ) + 1]
    data.table::setkey(data, period_number)

    return(invisible(data))
  } else {
    start_date <- base::min(dplyr::pull(data, {{ date_col }}), na.rm = TRUE)

    data <- data |>
      dplyr::mutate(
        period_number = base::floor(
          lubridate::interval(start_date, {{ date_col }}) / lubridate::days(1) / period_length
        ) + 1
      )
    return(data)
  }
}
#------------------------------------------------------------------
#' @method create_cutoff Week
#' @title [create_cutoff()] Week Based Periods
#' @inheritParams create_cutoff
create_cutoff.Week <- function(data, date_col, period_length) {
  if (inherits(data, "data.table")) {
    date_col <- rlang::as_name(rlang::enquo(date_col))
    start_date <- base::min(data[, get(date_col)])

    data[, period_number := base::floor(
      lubridate::interval(start_date, base::get(date_col)) / lubridate::weeks(1) / period_length
    ) + 1]
    data.table::setkey(data, period_number)

    return(invisible(data))
  } else {
    start_date <- base::min(dplyr::pull(data, {{ date_col }}), na.rm = TRUE)

    data <- data |>
      dplyr::mutate(
        period_number = base::floor(
          lubridate::interval(start_date, {{ date_col }}) / lubridate::weeks(1) / period_length
        ) + 1
      )
    return(data)
  }
}
#------------------------------------------------------------------

#' #' @method create_cutoff Month
#' @title [create_cutoff()] Month Based Periods
#' @inheritParams create_cutoff
#'
create_cutoff.Month <- function(data, date_col, month_col, period_length) {
  if (inherits(data, "data.table")) {
    date_col <- rlang::as_name(rlang::enquo(date_col))
    start_date <- base::min(data[, get(date_col)])

    month_col <- rlang::as_name(rlang::enquo(month_col))

    first_month <- data[order(date_col), get(month_col)][1]

    start_month <- lubridate::ymd(base::paste0(lubridate::year(start_date), "-", first_month, "-01"))

    data[, month_date := lubridate::ymd(
      base::paste0(lubridate::year(base::get(date_col)), "-", base::get(month_col), "-01")
    )]

    data[, period_number := base::floor(
      lubridate::interval(start_month, month_date) / base::months(1) / period_length
    ) + 1]

    data[, month_date := NULL]
    return(invisible(data))
  } else {
    start_date <- base::min(dplyr::pull(data, {{ date_col }}), na.rm = TRUE)
    first_month <- data |>
      dplyr::slice_min(order_by = {{ date_col }}, n = 1, with_ties = FALSE) |>
      dplyr::pull({{ month_col }})

    start_month <- lubridate::ymd(
      paste0(lubridate::year(start_date), "-", first_month, "-01")
    )
    data <- data |>
      dplyr::mutate(
        month_date = lubridate::ymd(paste0(
          lubridate::year({{ date_col }}), "-", {{ month_col }}, "-01"
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
