#' Precomputing Key Values for Outcome Imputation
#' @name imputation_precompute
#' @description Pre-computes key values required for the outcome imputation step of the Multi-Arm-Bandit
#' procedure. Calculates the probabilities of success for each treatment block (treatment arm + any blocking specified),
#' using the grouped means of the original experimental data. When `perfect_assignment` is FALSE, the average date of
#' success is calculated for each treatment block at every period.
#' @inheritParams single_mab_simulation
#' @returns A named list containing:
#' \itemize{
#' \item `original_summary`: The tibble(s)/data.table(s) containing the probability of success for each
#' treatment block, at each period.
#' \item `dates_summary`: A tibble/data.table containing the average success date for
#' each treatment block at each treatment period.
#' }
#' @details
#' [imputation_precompute()] is an optimization, meant to reduce the cost of calculating these variables
#' within the simulation loop. When `whole_experiment` is TRUE, `original_summary` is a single tibble/data.table,
#' and used through the simulation. When `whole_experiment` is FALSE, `original_summary` is a list of tibbles/data.tables,
#' each containing the cumulative probabilities of all periods up to the index i.
#'
#' If `perfect_assignment` is FALSE, `dates_summary` is not calculated, and is NULL.
#'
#' No covariates are used in the calculation, these are all simply grouped averages.
#' @keywords internal

imputation_precompute <- function(
  data,
  whole_experiment,
  perfect_assignment,
  data_cols
) {
  base::UseMethod("imputation_precompute", data)
}
#-------------------------------------------------------------------------------

#' @method imputation_precompute data.frame
#' @title [imputation_precompute()] for data.frames
#' @inheritParams imputation_precompute
#' @noRd

imputation_precompute.data.frame <- function(
  data,
  whole_experiment,
  perfect_assignment,
  data_cols
) {
  if (whole_experiment) {
    original_summary <- data |>
      dplyr::group_by(treatment_block) |>
      dplyr::summarize(
        success_rate = base::mean(!!data_cols$success_col$sym, na.rm = TRUE),
        .groups = "drop"
      ) |>
      dplyr::mutate(failure_rate = 1 - success_rate)
  } else if (!whole_experiment) {
    original_summary <- data |>
      dplyr::group_by(period_number, treatment_block) |>
      dplyr::summarize(
        count = dplyr::n(),
        n_success = base::sum(!!data_cols$success_col$sym),
        .groups = "drop",
      ) |>
      dplyr::arrange(period_number, treatment_block) |>
      dplyr::group_by(treatment_block) |>
      dplyr::mutate(
        cumulative_count = dplyr::lag(base::cumsum(count), default = 0),
        cumulative_success = dplyr::lag(base::cumsum(n_success), default = 0),
        success_rate = dplyr::if_else(
          cumulative_count > 0,
          (cumulative_success / cumulative_count),
          0
        )
      ) |>
      dplyr::ungroup() |>
      dplyr::select(period_number, treatment_block, success_rate) |>
      dplyr::mutate(failure_rate = 1 - success_rate) |>
      dplyr::group_split(period_number)
  } else {
    rlang::abort("Specify Logical for `whole_experiment`")
  }

  if (!perfect_assignment) {
    dates_summary <- data |>
      dplyr::group_by(treatment_block, period_number) |>
      dplyr::summarize(
        mean_date = base::mean(!!data_cols$success_date_col$sym, na.rm = TRUE),
        .groups = "drop"
      ) |>
      tidyr::pivot_wider(
        names_from = "treatment_block",
        values_from = "mean_date"
      )
  } else {
    dates_summary <- NULL
  }

  imputation_information <- list(
    success = original_summary,
    dates = dates_summary
  )

  return(imputation_information)
}
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#' @method imputation_precompute data.table
#' @title
#' [imputation_precompute()] for data.tables
#' @inheritParams imputation_precompute
#' @noRd

imputation_precompute.data.table <- function(
  data,
  whole_experiment,
  perfect_assignment,
  data_cols
) {
  if (whole_experiment) {
    original_summary <- data[,
      .(
        success_rate = base::mean(
          base::get(data_cols$success_col$name),
          na.rm = TRUE
        )
      ),
      by = treatment_block
    ]
    original_summary[, failure_rate := 1 - success_rate]

    data.table::setorder(original_summary, treatment_block)
  } else if (!whole_experiment) {
    original_summary <- data[,
      .(
        count = .N,
        n_success = base::sum(
          base::get(data_cols$success_col$name)
        )
      ),
      by = .(period_number, treatment_block)
    ]

    data.table::setorder(original_summary, period_number, treatment_block)

    original_summary[,
      `:=`(
        cumulative_count = data.table::shift(
          base::cumsum(count),
          type = "lag",
          fill = 0
        ),
        cumulative_success = data.table::shift(
          base::cumsum(n_success),
          type = "lag",
          fill = 0
        )
      ),
      by = treatment_block
    ]

    original_summary[,
      success_rate := data.table::fifelse(
        cumulative_count > 0,
        (cumulative_success / cumulative_count),
        0
      )
    ]

    original_summary <- original_summary[, .(
      period_number,
      treatment_block,
      success_rate
    )]

    original_summary[, failure_rate := 1 - success_rate]

    original_summary <- split(original_summary, by = "period_number")
    original_summary <- lapply(original_summary, \(x) {
      x[, period_number := NULL]
    })
  } else {
    rlang::abort("Specify Logical for `whole_experiment`")
  }

  if (!perfect_assignment) {
    dates_summary <- data.table::dcast(
      data[, .(
        period_number,
        treatment_block,
        base::get(data_cols$success_date_col$name)
      )],
      formula = period_number ~ treatment_block,
      fun.aggregate = \(x) base::mean(x, na.rm = TRUE),
      value.var = "V3"
    )
  } else {
    dates_summary <- NULL
  }

  imputation_information <- list(
    success = original_summary,
    dates = dates_summary
  )

  return(imputation_information)
}
#-------------------------------------------------------------------------------
#' @name imputation_preparation
#' @title Outcome Imputation Preparation
#' @description Executes all preparations necessary to impute outcomes for
#' each iteration of the simulation loop. Adds an additional column to the current data,
#' subsets necessary information from the [imputation_precompute()] output, and checks to ensure
#' compatibility with [randomizr::block_ra()].
#' @inheritParams get_past_results
#' @inheritParams run_mab_trial
#' @inheritParams impute_success
#' @returns A named list containing:
#' \itemize{
#' \item `current_data`: A tibble/data.table containing `impute_block` column to guide the outcome imputations
#' \item `impute_success`: A tibble/data.table object containing probabilities of success by `treatment_block` used to impute
#' outcomes. Subsetted from the [imputation_precompute()] output.
#' \item `impute_dates`: Named date vector by treatment condition, containing the dates of success
#' to impute if perfect_assignment is FALSE. Subsetted from the [imputation_precompute()] output.}
#'
#' @details
#' The goal of this function is to set up the imputation procedure and prevent
#' errors from occurring. [randomizr::block_ra()] does not see the names
#' of the probabilities passed per block, so the imputation information must be subsetted
#' to contain only the treatment blocks which exist in a given period.
#'
#' These checks are not implemented in `tryCatch()` block because they have to happen
#' in every iteration.
#'
#' `impute_block` is the observation's new treatment block, combining any
#' blocking variables with their new treatment assigned via the Multi-Arm-Bandit
#' procedure.
#'
#' @keywords internal

imputation_preparation <- function(
  current_data,
  block_cols,
  imputation_information,
  whole_experiment,
  blocking,
  perfect_assignment,
  current_period
) {
  if (data.table::is.data.table(current_data)) {
    if (blocking) {
      current_data[,
        impute_block := do.call(paste, c(.SD, sep = "_")),
        .SDcols = c("mab_condition", block_cols$name)
      ]
    } else {
      current_data[, impute_block := base::as.character(mab_condition)]
    }
  } else {
    if (blocking) {
      current_data$impute_block <- do.call(
        paste,
        c(current_data[, c("mab_condition", block_cols$name)], sep = "_")
      )
    } else {
      current_data$impute_block <- base::as.character(
        current_data$mab_condition
      )
    }
  }

  if (whole_experiment) {
    impute_success <- imputation_information[["success"]]
  } else {
    impute_success <- imputation_information[["success"]][[current_period]]
  }
  if (!perfect_assignment) {
    dates <- rlang::set_names(
      base::as.Date(base::as.numeric(imputation_information[["dates"]][
        current_period,
        -1
      ])),
      base::names(imputation_information[["dates"]][current_period, -1])
    )
  } else {
    dates <- NULL
  }

  impute_success <- check_impute(
    imputation_information = impute_success,
    current_data = current_data,
    current_period = current_period
  )

  return(list(
    current_data = current_data,
    impute_success = impute_success,
    impute_dates = dates
  ))
}
#-------------------------------------------------------------------------------
#' Checking Imputation Info
#' @description Subsets or adds to the tibble/data.frame created by [imputation_precompute()],
#' and sorts it to ensure compatibility with [randomizr::block_ra()].
#'
#' @name check_impute
#' @inheritParams get_past_results
#' @inheritParams impute_success
#' @param imputation_information The `success` element of the `imputation_information`
#' list created by [imputation_precompute()] for the given period.
#' @details
#' [randomizr::block_ra()] does not see the names
#' of the probabilities passed per block, so the imputation information must be subsetted
#' to only contain blocks which are present in a period, and sorted to comply with
#' [randomizr::block_ra()]'s internal ordering.
#'
#' When blocks are required but do not exist in the information provided it is added
#' to the tibble/data.table, with an estimated conditional probability of success as
#' the average across other blocks.
#'
#' When blocks are present but not required, they are removed from the
#' tibble/data.table.
#'
#' @keywords internal
check_impute <- function(imputation_information, current_data, current_period) {
  base::UseMethod("check_impute", imputation_information)
}
#--------------------------------------------------------------------------------
#' @method check_impute data.frame
#' @title [check_impute()] for data.frames
#' @inheritParams check_impute
#' @noRd

check_impute.data.frame <- function(
  imputation_information,
  current_data,
  current_period
) {
  mean_rate <- base::mean(imputation_information$success_rate)

  current_blocks <- stats::na.omit(current_data$impute_block[
    current_data$impute_req == 1
  ])
  imputation_blocks <- stats::na.omit(imputation_information$treatment_block)

  missing_blocks <- base::setdiff(current_blocks, imputation_blocks)

  blocks_to_remove <- base::setdiff(imputation_blocks, current_blocks)

  if (base::length(missing_blocks) > 0) {
    addition <- tibble::tibble(
      treatment_block = missing_blocks,
      success_rate = mean_rate,
      failure_rate = 1 - mean_rate
    )

    imputation_information <- dplyr::bind_rows(imputation_information, addition)
  }

  if (base::length(blocks_to_remove) > 0) {
    imputation_information <- imputation_information[
      !imputation_information$treatment_block %in% blocks_to_remove,
    ]
  }

  imputation_information <- imputation_information[
    !duplicated(imputation_information$treatment_block),
  ][order(imputation_information$treatment_block), ]

  return(imputation_information)
}
#-------------------------------------------------------------------------------
#' @method check_impute data.table
#' @title [check_impute()] for data.tables
#' @inheritParams check_impute
#' @noRd
check_impute.data.table <- function(
  imputation_information,
  current_data,
  current_period
) {
  mean_rate <- base::mean(imputation_information$success_rate)

  current_blocks <- stats::na.omit(current_data[impute_req == 1, impute_block])

  imputation_blocks <- stats::na.omit(imputation_information$treatment_block)

  missing_blocks <- base::setdiff(current_blocks, imputation_blocks)

  blocks_to_remove <- base::setdiff(imputation_blocks, current_blocks)

  if (base::length(missing_blocks) > 0) {
    addition <- data.table::data.table(
      treatment_block = missing_blocks,
      success_rate = mean_rate,
      failure_rate = 1 - mean_rate
    )

    imputation_information <- data.table::rbindlist(
      list(imputation_information, addition),
      use.names = TRUE
    )
  }

  if (base::length(blocks_to_remove) > 0) {
    imputation_information <- imputation_information[
      !treatment_block %in% blocks_to_remove,
    ]
  }

  imputation_information <- imputation_information[!duplicated(treatment_block)]
  data.table::setorder(imputation_information, treatment_block)

  return(invisible(imputation_information))
}
#------------------------------------------------------------------------------------

#' Imputing New Outcomes of Multi-Arm-Bandit Trial
#' @name impute_success
#' @description Imputes outcomes for the current treatment assignment period.
#' Uses [randomizr::block_ra()] to impute the outcomes for observations
#' who were assigned new treatments. The probabilities used to guide the imputation
#' of the outcomes are pre-computed using the existing data from the original randomized experiment.
#' @param current_data Updated tibble/data.frame object containing new treatments from [assign_treatments()].
#' @inheritParams run_mab_trial
#' @param prior_data A tibble/data.frame containing all the data from previous periods.
#' Used to join together at the end for the next iteration of the simulation.
#' @param imputation_info A tibble/data.frame containing conditional probability of success by treatment block, for each
#' combination that exists in `current_data`, calculated from the original experiment.
#' Passed to [randomizr::block_ra()] to impute outcomes.
#' @param dates Named date vector containing average success date by treatment block to impute new success dates for
#' observations whose change in treatment also changes their outcome from failure to success.
#' @param current_period Numeric value of length 1; current treatment wave of the simulation.
#' @inheritParams get_past_results
#' @inheritParams single_mab_simulation
#' @inheritParams cols
#' @details
#' When `perfect_assignment` is FALSE, dates of success are imputed according to the average
#' by each period and treatment block (treatment arm + any blocking). These imputations are required because
#' these observations do not currently have dates of success, as no success was observed during the original experiment.
#' Therefore if they go through the next iteration of the simulation without being imputed,
#' the new successes will still be treated as failues becasue of the date masking mechanism.
#'
#' Observations that were successful in the original experiment, got assigned a new treatment, and then
#' imputed as success again, will have their original date kept. This assumes that the treatment has no individual
#' treatment effect on the date of success, which may or may not be valid depending on the context of the
#' experiment.
#'
#' @seealso
#'* [imputation_preparation()]
#'* [imputation_precompute()]
#'* [randomizr::block_ra()]
#' @keywords internal
impute_success <- function(
  current_data,
  imputation_info,
  id_col,
  success_col,
  prior_data = NULL,
  perfect_assignment,
  dates = NULL,
  success_date_col,
  current_period = NULL
) {
  base::UseMethod("impute_success", current_data)
}
#-------------------------------------------------------------------------------
#' @inheritParams impute_success
#' @method impute_success data.frame
#' @title [impute_success()] for data.frames
#' @noRd

impute_success.data.frame <- function(
  current_data,
  imputation_info,
  id_col,
  success_col,
  prior_data,
  perfect_assignment,
  dates = NULL,
  success_date_col,
  current_period
) {
  if (base::any(current_data$impute_req == 1, na.rm = TRUE)) {
    filtered_data <- current_data[current_data$impute_req == 1, ]

    imputations <- randomizr::block_ra(
      blocks = filtered_data$impute_block,
      block_prob_each = imputation_info[, c("failure_rate", "success_rate")],
      num_arms = 2,
      conditions = c(0, 1),
      check_inputs = FALSE
    )

    filtered_data$mab_success <- imputations

    imputed <- dplyr::rows_update(current_data, filtered_data, by = id_col$name)

    imputed$mab_success <- base::ifelse(
      base::is.na(imputed$mab_success) & imputed$impute_req == 0,
      imputed[[success_col$name]],
      imputed$mab_success
    )
  } else {
    imputed <- current_data
    imputed$mab_success <- imputed[[success_col$name]]
  }

  if (!perfect_assignment) {
    imputed$new_success_date <- dplyr::case_when(
      imputed$impute_req == 0 |
        (imputed[[success_col$name]] == 0 & imputed$mab_success == 1) ~
        imputed[[success_date_col$name]],
      imputed$mab_success == 1 & imputed[[success_col$name]] == 0 ~
        dates[imputed$impute_block],
      imputed$mab_success == 0 | TRUE ~ base::as.Date(NA)
    )
  }
  data <- dplyr::rows_update(prior_data, imputed, by = id_col$name)
  return(data)
}
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#' @inheritParams impute_success
#' @method impute_success data.table
#' @title [impute_success()] for data.tables
#' @noRd
impute_success.data.table <- function(
  current_data,
  imputation_info,
  id_col,
  success_col,
  prior_data,
  perfect_assignment,
  dates = NULL,
  success_date_col,
  current_period
) {
  if (current_data[impute_req == 1, .N] > 0) {
    blocks <- current_data[impute_req == 1, impute_block]

    imputations <- randomizr::block_ra(
      blocks = blocks,
      block_prob_each = imputation_info[, .(failure_rate, success_rate)],
      num_arms = 2,
      conditions = c(0, 1),
      check_inputs = FALSE
    )

    current_data[impute_req == 1, mab_success := imputations]
    current_data[impute_req == 0, mab_success := base::get(success_col$name)]
  } else {
    current_data[, mab_success := base::get(success_col$name)]
  }

  prior_data[
    period_number == current_period,
    `:=`(
      mab_condition = current_data[, mab_condition],
      impute_req = current_data[, impute_req],
      impute_block = current_data[, impute_block],
      mab_success = current_data[, mab_success]
    )
  ]

  if (!perfect_assignment) {
    prior_data[
      period_number == current_period,
      new_success_date := data.table::fcase(
        impute_req == 0 | (get(success_col$name) == 0 & mab_success == 1),
        get(success_date_col$name),
        mab_success == 1 & get(success_col$name) == 0,
        dates[impute_block],
        mab_success == 0 | TRUE,
        base::as.Date(NA)
      )
    ]
  }

  return(invisible(prior_data))
}
