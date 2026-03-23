#' Precomputing Key Values for Outcome Imputation
#' @name precompute_imputation
#' @description Pre-computes key values required for the outcome imputation step of the MAB
#' procedure. Calculates the probabilities of success for each treatment block (treatment arm + any blocking specified),
#' using the grouped means of the original experimental data. When `delayed_feedback = TRUE`, the average date of
#' success is calculated for each treatment block at every period.
#' @inheritParams mab_from_rct
#' @inheritParams prep_rct_data
#' @returns A named list containing:
#' \itemize{
#' \item `original_summary`: The matrix or list of matrices containing the probability of success for each
#' treatment block, at each period.
#' \item `dates_summary`: A list of vectors containing the average success date for
#' each treatment block at each treatment period.
#' }
#' @details
#' [precompute_imputation()] is an optimization, meant to reduce the cost of calculating these variables
#' within the simulation loop. When `whole_experiment = TRUE`, `original_summary` is a single matrix,
#' and used through the simulation. When `whole_experiment = FALSE`, `original_summary` is a list of matrices,
#' each containing the cumulative probabilities of all periods up to the index `i`.
#'
#' If `perfect_assignment = FALSE`, `dates_summary` is not calculated, and is `NULL`.
#'
#' No covariates are used in the calculation, these are all simply grouped means.
#' @keywords internal

precompute_imputation <- function(
  data,
  whole_experiment,
  delayed_feedback,
  data_cols
) {
  UseMethod("precompute_imputation", data)
}
#-------------------------------------------------------------------------------

#' @method precompute_imputation data.frame
#' @title [precompute_imputation()] for data.frames
#' @inheritParams precompute_imputation
#' @noRd

precompute_imputation.data.frame <- function(
  data,
  whole_experiment,
  delayed_feedback,
  data_cols
) {
  original_summary <- if (whole_experiment) {
    data |>
      dplyr::group_by(treatment_block) |>
      dplyr::summarize(
        success_rate = mean(
          !!data_cols[["success_col"]][["sym"]],
          na.rm = TRUE
        ),
        .groups = "drop"
      ) |>
      dplyr::mutate(failure_rate = 1 - success_rate) |>
      summary_to_matrix()
  } else {
    data |>
      dplyr::group_by(period_number, treatment_block) |>
      dplyr::summarize(
        count = dplyr::n(),
        n_success = sum(!!data_cols[["success_col"]][["sym"]]),
        .groups = "drop",
      ) |>
      dplyr::arrange(period_number, treatment_block) |>
      dplyr::group_by(treatment_block) |>
      dplyr::mutate(
        cumulative_count = dplyr::lag(cumsum(count), default = 0),
        cumulative_success = dplyr::lag(cumsum(n_success), default = 0),
        success_rate = dplyr::if_else(
          cumulative_count > 0,
          (cumulative_success / cumulative_count),
          0
        )
      ) |>
      dplyr::ungroup() |>
      dplyr::mutate(failure_rate = 1 - success_rate) |>
      dplyr::group_split(period_number) |>
      lapply(\(df) {
        summary_to_matrix(df)
      })
  }
  dates_summary <- if (delayed_feedback) {
    data |>
      dplyr::group_by(treatment_block, period_number) |>
      dplyr::summarize(
        mean_date = mean(
          !!data_cols[["success_date_col"]][["sym"]],
          na.rm = TRUE
        ),
        .groups = "drop"
      ) |>
      dplyr::arrange(period_number) |>
      dplyr::group_split(period_number) |>
      lapply(\(df) {
        as.Date(as_named_vec(
          df,
          val = "mean_date",
          name = "treatment_block"
        ))
      })
  } else {
    NULL
  }

  imputation_information <- list(
    success = original_summary,
    dates = dates_summary
  )

  return(imputation_information)
}
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#' @method precompute_imputation data.table
#' @title
#' [precompute_imputation()] for `data.table`s
#' @inheritParams precompute_imputation
#' @noRd

precompute_imputation.data.table <- function(
  data,
  whole_experiment,
  delayed_feedback,
  data_cols
) {
  original_summary <- if (whole_experiment) {
    rct_sum <- data[,
      .(
        success_rate = mean(
          get(data_cols[["success_col"]][["name"]]),
          na.rm = TRUE
        )
      ),
      by = treatment_block
    ]
    rct_sum[, failure_rate := 1 - success_rate]
    data.table::setorder(rct_sum, treatment_block)
    summary_to_matrix(rct_sum)
  } else {
    rct_sum <- data[,
      .(
        count = .N,
        n_success = sum(
          get(data_cols[["success_col"]][["name"]])
        )
      ),
      by = .(period_number, treatment_block)
    ]

    data.table::setorder(rct_sum, period_number, treatment_block)

    rct_sum[,
      `:=`(
        cumulative_count = data.table::shift(
          cumsum(count),
          type = "lag",
          fill = 0
        ),
        cumulative_success = data.table::shift(
          cumsum(n_success),
          type = "lag",
          fill = 0
        )
      ),
      by = treatment_block
    ]

    rct_sum[,
      success_rate := data.table::fifelse(
        cumulative_count > 0,
        (cumulative_success / cumulative_count),
        0
      )
    ][, failure_rate := 1 - success_rate]

    split(rct_sum, by = "period_number") |>
      lapply(\(df) {
        summary_to_matrix(df)
      })
  }

  dates_summary <- if (delayed_feedback) {
    data[,
      .(mean_date = mean(get(data_cols[["success_date_col"]][["name"]]))),
      by = .(period_number, treatment_block)
    ] |>
      split(by = "period_number") |>
      lapply(\(df) {
        as.Date(as_named_vec(
          df,
          val = "mean_date",
          name = "treatment_block"
        ))
      })
  } else {
    NULL
  }

  imputation_information <- list(
    success = original_summary,
    dates = dates_summary
  )

  return(imputation_information)
}
#-------------------------------------------------------------------------------
#' Convert Treatment Block Summary to Matrix
#' @name summary_to_matrix
#' @description Converts a summarized data.frame or data.table containing
#' `treatment_block`, `success_rate`, and `failure_rate` columns into a
#' named matrix for use with [randomizr::block_ra()].
#' @param df A `data.frame` or `data.table` with columns `treatment_block`,
#' `success_rate`, and `failure_rate`.
#' @returns A numeric matrix with row names equal to `treatment_block` and
#' columns `failure_rate` and `success_rate`.
#' @keywords internal
summary_to_matrix <- function(df) {
  UseMethod("summary_to_matrix", df)
}

#' @method summary_to_matrix data.frame
#' @noRd
summary_to_matrix.data.frame <- function(df) {
  m <- as.matrix(df[, c("failure_rate", "success_rate")])
  rownames(m) <- df[["treatment_block"]]
  m
}

#' @method summary_to_matrix data.table
#' @noRd
summary_to_matrix.data.table <- function(df) {
  m <- as.matrix(df[, .(failure_rate, success_rate)])
  rownames(m) <- df[["treatment_block"]]
  m
}

#-------------------------------------------------------------------------------
#' @name imputation_prep
#' @title Outcome Imputation Preparation
#' @description Executes all preparations necessary to impute outcomes for
#' each iteration of the simulation loop. Adds an additional column to the current data,
#' subsets necessary information from the [precompute_imputation()] output, and checks to ensure
#' compatibility with [randomizr::block_ra()].
#' @inheritParams compute_prior
#' @inheritParams mab_loop
#' @inheritParams impute_outcomes
#' @param block_cols Names of the blocking columns
#' @returns A named list containing:
#' \itemize{
#' \item `current_data`: A `tibble` or `data.table` containing `impute_block` column to guide the outcome imputations
#' \item `impute_success`: A matrix object containing probabilities of success by `treatment_block` used to impute
#' outcomes. Subsetted from the [precompute_imputation()] output.
#' \item `impute_dates`: Named date vector by treatment condition, containing the dates of success
#' to impute if delayed_feedback is FALSE. Subsetted from the [precompute_imputation()] output.}
#'
#' @details
#' The goal of this function is to set up the imputation procedure and prevent
#' errors from occurring. [randomizr::block_ra()] does not see the names
#' of the probabilities passed per block, so the imputation information must be subsetted
#' to contain only the treatment blocks which exist in a given period.
#'
#' `impute_block` is the observation's new treatment block, combining any
#' blocking variables with their new treatment assigned via the Multi-Arm-Bandit
#' procedure.
#'
#' @keywords internal

prep_imputation <- function(
  current_data,
  block_cols,
  imputation_information,
  whole_experiment,
  blocking,
  delayed_feedback,
  current_period
) {
  if (data.table::is.data.table(current_data)) {
    if (blocking) {
      current_data[,
        impute_block := do.call(paste, c(.SD, sep = "_")),
        .SDcols = c("mab_condition", block_cols)
      ]
    } else {
      current_data[, impute_block := as.character(mab_condition)]
    }
  } else {
    if (blocking) {
      current_data[["impute_block"]] <- do.call(
        paste,
        c(
          current_data[, c(
            "mab_condition",
            block_cols
          )],
          sep = "_"
        )
      )
    } else {
      current_data[["impute_block"]] <- as.character(
        current_data[["mab_condition"]]
      )
    }
  }

  impute_success <- if (whole_experiment) {
    imputation_information[["success"]]
  } else {
    imputation_information[["success"]][[current_period]]
  }

  dates <- if (delayed_feedback) {
    imputation_information[["dates"]][[current_period]]
  } else {
    NULL
  }
  impute_idx <- which(current_data[["impute_req"]] == 1)

  impute_success <- check_impute(
    impute_success = impute_success,
    current_data = current_data,
    impute_idx = impute_idx
  )

  return(list(
    current_data = current_data,
    impute_success = impute_success,
    impute_dates = dates,
    impute_idx = impute_idx
  ))
}
#-------------------------------------------------------------------------------
#' Checking Imputation Info
#' @description Subsets or adds to the `tibble`/`data.table` created by [precompute_imputation()],
#' and sorts it to ensure compatibility with [randomizr::block_ra()].
#'
#' @name check_impute
#' @inheritParams compute_prior
#' @inheritParams impute_outcomes
#' @param impute_success The `success` element of the `imputation_information`
#' list created by [precompute_imputation()] for the given period.
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
#' @returns Proper `impute_success` matrix as required by [randomizr::block_ra()]
#'
#' @keywords internal
check_impute <- function(impute_success, current_data, impute_idx) {
  current_blocks <- stats::na.omit(current_data[["impute_block"]][impute_idx])
  imputation_blocks <- rownames(impute_success)

  missing_blocks <- setdiff(current_blocks, imputation_blocks)
  blocks_to_remove <- setdiff(imputation_blocks, current_blocks)

  if (length(missing_blocks) > 0) {
    mean_rate <- mean(impute_success[, "success_rate"])
    n_miss <- length(missing_blocks)
    addition <- matrix(
      c(rep(1 - mean_rate, n_miss), rep(mean_rate, n_miss)),
      nrow = n_miss,
      ncol = 2,
      dimnames = list(missing_blocks, c("failure_rate", "success_rate"))
    )
    impute_success <- rbind(impute_success, addition)
  }

  if (length(blocks_to_remove) > 0) {
    keep <- !rownames(impute_success) %in% blocks_to_remove
    impute_success <- impute_success[keep, , drop = FALSE]
  }

  return(impute_success[
    order(rownames(impute_success)),
    ,
    drop = FALSE
  ])
}
#------------------------------------------------------------------------------------

#' Imputing New Outcomes of Multi-Arm-Bandit Trial
#' @name impute_outcomes
#' @description Imputes outcomes for the current treatment assignment period.
#' Uses [randomizr::block_ra()] to impute the outcomes for observations
#' who were assigned new treatments. The probabilities used to guide the imputation
#' of the outcomes are pre-computed using the existing data from the original randomized experiment.
#' @inheritParams prep_rct_data
#' @inheritParams mab_from_rct
#' @param imputation_info List containing all necessary information for imputation, generated each period by [prep_imputation()]
#' @param impute_idx Index of rows in `current_data` that need to be imputed.
#' @param idx Index of rows imputed in the period, used to update the original data.
#' @returns Updated `data` object with all the updates from the current period of the simulation
#' @details
#' When `delayed_feedback = TRUE`, dates of success are imputed according to the average
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
#'* [prep_imputation()]
#'* [precompute_imputation()]
#'* [randomizr::block_ra()]
#' @keywords internal
impute_outcomes <- function(
  data,
  imputation_info,
  success_col,
  success_date_col = NULL,
  delayed_feedback,
  impute_idx,
  idx
) {
  UseMethod("impute_outcomes", current_data)
}

#' Compute Imputations
#' @name compute_impute
#' @description
#' Performs all `data.table`/`data.frame` agnostic portions of `impute_sucess`
#' @inheritParams impute_outcomes
#' @returns Numeric vector of imputed outcomes

compute_impute <- function(imputation_info) {
  current_data <- imputation_info[["current_data"]]
  imputation_means <- imputation_info[["impute_success"]]
  impute_idx <- imputation_info[["impute_idx"]]
  non_impute_idx <- setdiff(
    seq_len(nrow(current_data)),
    impute_idx
  )

  imputations <- vector(
    mode = "numeric",
    length = nrow(current_data)
  )
  imputations[non_impute_idx] <- current_data[[success_col]][non_impute_idx]

  if (length(impute_idx) > 0) {
    imputations[impute_idx] <- randomizr::block_ra(
      blocks = current_data[["impute_block"]][impute_idx],
      block_prob_each = imputation_means,
      num_arms = 2,
      conditions = c(0, 1),
      check_inputs = FALSE
    )
  }
  return(imputations)
}
#-------------------------------------------------------------------------------
#' @inheritParams impute_outcomes
#' @method impute_outcomes data.frame
#' @title [impute_outcomes()] for `data.frames`
#' @noRd

impute_outcomes.data.frame <- function(
  data,
  imputation_info,
  success_col,
  success_date_col,
  delayed_feedback,
  impute_idx,
  idx
) {
  current_data <- imputation_info[["current_data"]]
  current_data[["mab_success"]] <- compute_impute(imputation_info)
  if (delayed_feedback) {
    dates <- imputation_info[["impute_dates"]]

    current_data[["new_success_date"]] <- dplyr::case_when(
      current_data[["impute_req"]] == 0 ~ current_data[[success_date_col]],
      current_data[["mab_success"]] == 1 &
        current_data[[success_col]] == 0 ~ dates[
        current_data[["impute_block"]]
      ],
      .default = as.Date(NA)
    )
  }

  data[idx, ] <- current_data
  return(data)
}
#-------------------------------------------------------------------------------
#' @inheritParams impute_outcomes
#' @method impute_outcomes data.table
#' @title [impute_outcomes()] for `data.table`s
#' @noRd
impute_outcomes.data.table <- function(
  data,
  imputation_info,
  success_col,
  success_date_col,
  delayed_feedback,
  impute_idx,
  idx
) {
  current_data <- imputation_info[["current_data"]]
  current_data[, mab_success := compute_impute(imputation_info)]
  modified_cols <- c(
    "mab_condition",
    "mab_success",
    "impute_req",
    "impute_block",
    "assignment_type"
  )

  if (delayed_feedback) {
    dates <- imputation_info[["impute_dates"]]
    current_data[,
      new_success_date := data.table::fcase(
        impute_req == 0                          , get(success_date_col) ,
        mab_success == 1 & get(success_col) == 0 , dates[impute_block]   ,
        default = as.Date(NA)
      )
    ]
    modified_cols <- c(modified_cols, "new_success_date")
  }

  data[idx, (modified_cols) := current_data[, ..modified_cols]]

  return(invisible(data))
}
