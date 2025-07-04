#' @title Prepare Data for Multi-Arm-Bandit Trials
#' @name mab_prepare
#' @inheritParams single_mab_simulation
#' @inheritParams cols
#'
#' @description Transforms user-provided input data for use with [mab_simulation()]. Adds
#' additional columns to ensure compatibility.
#'
#' @returns A prepared data object of the same class as `data`,
#'  containing all the columns required by [mab_simulation()].
#'
#'
#' @seealso
#'* [single_mab_simulation()]
#'* [multiple_mab_simulation()]
#'* [mab_simulation()]
#'* [create_cutoff()]
#'* [create_new_cols()]

mab_prepare <- function(data = data,
                        data_cols = data_cols,
                        block_cols = block_cols,
                        time_unit,
                        period_length,
                        perfect_assignment,
                        assignment_method,
                        blocking,
                        verbose) {
  verbose_log(verbose, "Preparing Data")

  data <- create_cutoff(
    data = data,
    data_cols = data_cols,
    period_length = period_length,
    assignment_method = assignment_method,
    time_unit = time_unit
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


  return(data)
}

#--------------------------------------------------------------------------------
