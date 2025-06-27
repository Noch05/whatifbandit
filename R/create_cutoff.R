#' Create Treatment Wave Cutoffs
#' @name create_cutoff
#' @description Used during [run_mab_trial()] to prepare data for the Multi-Arm-Bandit procedure, by assigning treatment
#' waves and initializing new columns
#'
#' @inheritParams single_mab_simulation
#'
#' @return data frame containing the original data with 5 additional columns.
#' \item{period_number}{Integer identifying the treatment wave for each observation}
#' \item{mab_success}{New variable to hold new success from Multi-arm bandit procedure, NA until assigned.}
#' \item{mab_condition}{New variable to hold new treatment condition from Multi-arm bandit procedure, NA until assigned.}
#' \item{impute_req}{Binary indicator for imputation requirement, NA until assigned}
#' \item{new_recert_date}{New variable to new recertification date from Multi-arm bandit procedure, NA until assigned.}
#' @seealso
#' *[run_mab_trial()]
#' @export



create_cutoff <- function(data, time_unit, date_col, month_col = NULL, period_length = NULL,
                          success_col, condition_col, success_date_col, perfect_assignment) {
  start_date <- base::min(dplyr::pull(data, {{ date_col }}), na.rm = TRUE)

  if (time_unit == "Individual") {
    data <- data |>
      dplyr::mutate(period_number = dplyr::row_number())
  } else if (time_unit == "Month") {
    first_month <- data |>
      dplyr::slice_min(order_by = {{ date_col }}, n = 1, with_ties = FALSE) |>
      dplyr::pull({{ month_col }})

    start_month <- lubridate::ymd(
      paste0(lubridate::year(start_date), "-", first_month, "-01")
    )

    data <- data |>
      dplyr::mutate(
        month_date = lubridate::ymd(paste0(lubridate::year({{ date_col }}), "-", {{ month_col }}, "-01")),
        period_number = floor(lubridate::interval(start_month, month_date) / base::months(1) / period_length)
      ) |>
      dplyr::select(-month_date)
  } else if (time_unit == "Day") {
    data <- data |>
      dplyr::mutate(
        period_number = floor(lubridate::interval(start_date, {{ date_col }}) / lubridate::days(1) / period_length) + 1
      )
  } else if (time_unit == "Week") {
    data <- data |>
      dplyr::mutate(
        period_number = floor(lubridate::interval(start_date, {{ date_col }}) / lubridate::weeks(1) / period_length) + 1
      )
  } else {
    rlang::abort("Invalid Time-Unit, Valid units: Day, Week, Month, Individual")
  }

  data <- data |>
    dplyr::mutate(
      period_number = base::match(period_number, base::sort(base::unique(period_number))),
      mab_success = dplyr::if_else(period_number == 1, {{ success_col }}, NA_real_),
      mab_condition = dplyr::if_else(period_number == 1, {{ condition_col }}, NA_character_),
      impute_req = dplyr::if_else(period_number == 1, 0, NA_real_)
    )

  if (!perfect_assignment) {
    data <- data |>
      dplyr::mutate(new_success_date = dplyr::if_else(period_number == 1, {{ success_date_col }}, NA))
  }


  return(data)
}
