#'@title Prepare Data for Multi Arm Bandit Trials
#'@name mab_prepare
#'@inheritParams single_mab_simulation
#'
#'@description This function acts as an internal helper to [single_mab_simulation()]
#'and [multiple_mab_simulation()] by transforming user input data
#'into data that can be used by [mab_simulation()].
#'
#'
#'@seealso
#'* [single_mab_simulation()]
#'* [multiple_mab_simulation()]
#'* [mab_simulation()]
#'* [create_cutoff()]
#'
#'@export



mab_prepare <- function(data, date_col, time_unit,
                        period_length, success_col,
                        condition_col,
                        success_date_col,
                        month_col,
                        perfect_assignment, blocking,
                        block_cols) {


  # Create cutoffs
  data <- create_cutoff(
    data = data,
    date_col = {{ date_col }},
    time_unit = time_unit,
    period_length = period_length,
    success_col = {{ success_col }},
    condition_col = {{ condition_col }},
    success_date_col = {{ success_date_col }},
    month_col = {{ month_col }},
    perfect_assignment = perfect_assignment
  )

  # If blocking, create block and treatment_block columns
  if (blocking) {
    if (base::is.null(block_cols)) stop("If blocking is TRUE, blocking variables must be specified")
    data <- data |>
      tidyr::unite("block", tidyselect::all_of(block_cols), sep = "_", remove = FALSE) |>
      tidyr::unite("treatment_block", c({{ condition_col }}, tidyselect::all_of(block_cols)), sep = "_", remove = FALSE)
  } else {
    data <- data |>
      dplyr::mutate(treatment_block = {{ condition_col }})
  }
  return(data)
}
