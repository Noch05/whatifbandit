#' @title Prepare Data for Multi Arm Bandit Trials
#' @name mab_prepare
#' @inheritParams single_mab_simulation
#'
#' @description This function acts as an internal helper to [single_mab_simulation()]
#' and [multiple_mab_simulation()] by transforming user input data
#' into data that can be used by [mab_simulation()].
#'
#' @returns Prepared data.frame containing the columns required by [mab_simulation()]
#'
#'
#' @seealso
#'* [single_mab_simulation()]
#'* [multiple_mab_simulation()]
#'* [mab_simulation()]
#'* [create_cutoff()]
#'* [create_new_cols()]
#' @export

mab_prepare <- function(data, date_col, time_unit,
                        period_length, success_col,
                        condition_col,
                        success_date_col = NULL,
                        month_col,
                        perfect_assignment, blocking,
                        block_cols, assignment_method, verbose) {
  verbose_log(verbose, "Preparing Data")

  custom_class <- switch(assignment_method,
    "Date" = {
      switch(time_unit,
        "Day" = c("Day", class(data)),
        "Week" = c("Week", class(data)),
        "Month" = c("Month", class(data)),
        rlang::abort("Invalid time_unit. Valid units are: 'Day', 'Week', 'Month'")
      )
    },
    "Batch" = c("Batch", class(data)),
    "Individual" = c("Individual", class(data)),
    rlang::abort("Invalid assignment_method. Valid methods are: 'Individual', 'Batch', 'Date'")
  )

  if (inherits(data, "data.table")) {
    data.table::setattr(data, "class", custom_class)
  } else {
    base::class(data) <- custom_class
  }



  data <- create_cutoff(
    data = data,
    date_col = {{ date_col }},
    period_length = period_length,
    month_col = {{ month_col }}
  )

  data <- create_new_cols(
    data = data,
    success_date_col = {{ success_date_col }},
    perfect_assignment = perfect_assignment,
    blocking = blocking,
    block_cols = block_cols,
    success_col = {{ success_col }},
    condition_col = {{ condition_col }}
  )

  if (inherits(data, "data.table")) {
    data.table::setattr(data, "class", c("data.table", "data.frame"))
  } else {
    base::class(data) <- setdiff(class(data), c(
      "Day", "Week",
      "Month", "Individual",
      "Batch"
    ))
  }

  return(data)
}

#--------------------------------------------------------------------------------
