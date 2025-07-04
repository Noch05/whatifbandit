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
#' \item{mab_success}{New variable to hold new success from Multi-arm bandit procedure, NA until assigned.}
#' \item{mab_condition}{New variable to hold new treatment condition from Multi-arm bandit procedure, NA until assigned.}
#' \item{impute_req}{Binary indicator for imputation requirement, NA until assigned}
#' \item{new_success_date}{New variable to new recertification date from Multi-arm bandit procedure, NA until assigned.}
#' \item{block}{New variable indicating the variables to block by for assignment}
#' \item{treatment_block}{New variable combining block with original treatment condition}
#'
#' @seealso
#' *[create_cutoff()]
#' *[mab_prepare()]
#'
#'
create_new_cols <- function(data,
                            data_cols,
                            block_cols,
                            perfect_assignment) {
  base::UseMethod("create_new_cols")
}
# --------------------------------------------------

#' @title [create_new_cols()] for Tibbles
#' @method create_new_cols tbl_df
#' @inheritParams create_new_cols

create_new_cols.tbl_df <- function(data,
                                   data_cols,
                                   blocking,
                                   perfect_assignment) {
  data <- data |>
    dplyr::mutate(
      period_number = base::match(period_number, base::sort(base::unique(period_number))),
      mab_success = dplyr::if_else(period_number == 1, {{ success_col }}, NA_real_),
      mab_condition = dplyr::if_else(period_number == 1, {{ condition_col }}, NA_character_),
      impute_req = dplyr::if_else(period_number == 1, 0, NA_real_),
      impute_block = NA_character_
    )

  if (!perfect_assignment) {
    data <- data |>
      dplyr::mutate(new_success_date = dplyr::if_else(period_number == 1, {{ success_date_col }}, NA))
  }

  if (blocking) {
    if (base::is.null(block_cols)) {
      rlang::abort("If blocking is TRUE, blocking variables must be specified")
    }

    data <- data |>
      dplyr::mutate(
        block = do.call(paste, c(dplyr::across(tidyselect::all_of(block_cols)), sep = "_")),
        treatment_block = do.call(paste, c(dplyr::across(c({{ condition_col }}, tidyselect::all_of(block_cols))), sep = "_"))
      )
  } else {
    data <- data |>
      dplyr::mutate(treatment_block = {{ condition_col }})
  }
  return(data)
}
#---------------------------------------------------------------------------------
#' @title [create_new_cols()] for Data.frames
#' @method create_new_cols data.frame
#' @inheritParams create_new_cols
#'
create_new_cols.data.frame <- function(data,
                                       data_cols,
                                       blocking,
                                       perfect_assignment) {
  data <- create_new_cols.tbl_df(
    tibble::as_tibble(data),
    data_cols = data_cols,
    blocking = blocking,
    block_cols = block_cols,
    perfect_assignment = perfect_assignment
  )

  return(data)
}
#---------------------------------------------------------------------------------
#' @title [create_new_cols()] for Data.tables
#' @method create_new_cols data.table
#' @inheritParams create_new_cols

create_new_cols.data.table <- function(data,
                                       data_cols,
                                       blocking,
                                       block_cols,
                                       perfect_assignment) {
  condition_col_name <- rlang::as_name(rlang::enquo(condition_col))
  success_col_name <- rlang::as_name(rlang::enquo(success_col))

  data[, period_number := base::match(period_number, base::sort(base::unique(period_number)))][
    period_number == 1, `:=`(
      mab_success = base::get(success_col_name),
      mab_condition = base::get(condition_col_name),
      impute_req = 0,
      impute_block = NA_character_
    )
  ]
  if (!perfect_assignment) {
    data[period_number == 1, new_success_date := base::get(rlang::as_name(rlang::enquo(success_date_col)))]
  }


  if (blocking) {
    if (base::is.null(block_cols)) {
      rlang::abort("If blocking is TRUE, blocking variables must be specified")
    }

    data[, block := base::do.call(base::paste, c(.SD, sep = "_")), .SDcols = block_cols]
    data[, treatment_block := base::do.call(paste, c(.SD, sep = "_")), .SDcols = c(condition_col_name, block_cols)]
  } else {
    data[, treatment_block := get(condition_col_name)]
  }
  return(invisible(data))
}
