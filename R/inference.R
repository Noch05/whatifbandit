#' Calculate Observation Level AIPW For Each Treatment Condition
#' @name compute_iaipw
#' @description Calculates the augmented inverse probability weighted estimate (AIPW) of treatment
#' success for each observation and treatment (i.e. on the level of a single unit), and returns the final IPW weights
#' for each observation, (i.e. the reciprocal specific weight for the treatment they were assigned)
#'
#' @param periods Numeric value of length 1; number of total periods in the simulation.
#' @param assignment_probs A `tibble`/`data.table` containing the probabilities of being
#' assigned each treatment at a given period.
#' @inheritParams run_mab
#' @param cluster_col Name of the column holding the clustering index
#'
#' @returns A `tibble`/`data.frame`, containing the subset of `data`
#' used in the MAB trial, along with new columns for probabilities of assignment to each treatment,
#' aipw, and ipw weights for each observation.
#' @details
#' The specification for the individual AIPW estimates can be found
#' in \href{https://www.pnas.org/doi/full/10.1073/pnas.2014602118}{Hadad et al. (2021)}. The
#' formulas in equation 5, formed the basis for this function's calculations. Here
#' the regression adjustment used is the grouped mean of success by treatment, up until
#' the current period of estimation (so at period 5, the grouped mean would be calculated
#' using the results from periods 1 through 4).
#'
#'
#' @references
#' Hadad, Vitor, David A. Hirshberg, Ruohan Zhan, Stefan Wager, and Susan Athey. 2021.
#' "Confidence Intervals for Policy Evaluation in Adaptive Experiments." \emph{Proceedings of the National Academy of Sciences of the United States of America} 118
#' (15): e2014602118. \doi{10.1073/pnas.2014602118}.
#'
#' @keywords internal
compute_iaipw <- function(
  data,
  assignment_probs,
  periods,
  conditions
) {
  UseMethod("compute_iaipw", data)
}
#-------------------------------------------------------------------------------
#' @method compute_iaipw data.frame
#' @title
#' [compute_iaipw()] for `data.frame`s
#' @inheritParams compute_iaipw
#' @noRd

compute_iaipw.data.frame <- function(
  data,
  assignment_probs,
  periods,
  conditions
) {
  new_cols <- paste0("aipw_", conditions)
  data[new_cols] <- NA_real_

  prior_data <- data |>
    dplyr::group_by(period_number, mab_condition) |>
    dplyr::summarize(
      successes = sum(mab_success, na.rm = TRUE),
      trials = dplyr::n(),
      .groups = "drop"
    )
  names(assignment_probs) <- c(
    "period_number",
    paste0(names(assignment_probs)[-1], "_assign_prob")
  )

  data <- expand.grid(
    period_number = seq_len(periods),
    mab_condition = conditions
  ) |>
    dplyr::left_join(prior_data, by = c("period_number", "mab_condition")) |>
    dplyr::mutate(dplyr::across(
      c(successes, trials),
      ~ tidyr::replace_na(.x, 0)
    )) |>
    dplyr::arrange(mab_condition, period_number) |>
    dplyr::group_by(mab_condition) |>
    dplyr::mutate(
      cumulative_successes = dplyr::lag(cumsum(successes), default = 0),
      cumulative_trials = dplyr::lag(cumsum(trials), default = 0),
      prior_period_success_rate = dplyr::if_else(
        cumulative_trials > 0,
        cumulative_successes / cumulative_trials,
        0
      )
    ) |>
    dplyr::select(period_number, mab_condition, prior_period_success_rate) |>
    tidyr::pivot_wider(
      names_from = mab_condition,
      values_from = "prior_period_success_rate",
      names_prefix = "prior_rate_"
    ) |>
    dplyr::right_join(data, by = "period_number") |>
    dplyr::select(
      !!!rlang::syms(names(data)),
      tidyr::starts_with("prior_rate_")
    ) |>
    dplyr::left_join(assignment_probs, by = "period_number")

  for (condition in conditions) {
    probability <- data[[sprintf("%s_assign_prob", condition)]]
    mhat <- data[[sprintf("prior_rate_%s", condition)]]

    data[[sprintf("aipw_%s", condition)]] <- ifelse(
      data[["mab_condition"]] == condition,
      (data[["mab_success"]] / probability) + (1 - (1 / probability)) * mhat,
      mhat
    )
  }

  data[["true_assign_prob"]] <- data[cbind(
    seq_len(nrow(data)),
    match(paste0(data[["mab_condition"]], "_assign_prob"), names(data))
  )]
  data[["ipw_weights"]] <- 1 / data[["true_assign_prob"]]

  check <- sum(is.na(data[, new_cols]))

  if (check != 0) {
    warning(paste0(check, " Individual AIPW Scores are NA"))
  }
  return(data)
}
# ------------------------------------------------------------------------------
#' @method compute_iaipw data.table
#' @title [compute_iaipw()] for `data.table`s
#' @inheritParams compute_iaipw
#' @noRd

compute_iaipw.data.table <- function(
  data,
  assignment_probs,
  periods,
  conditions
) {
  new_cols <- paste0("aipw_", conditions)
  data[, (new_cols) := NA_real_]

  prior_data <- data[,
    .(
      successes = sum(mab_success),
      trials = .N
    ),
    by = c("mab_condition", "period_number")
  ]
  data.table::setkey(prior_data, period_number)

  full_grid <- data.table::CJ(
    period_number = seq_len(periods),
    mab_condition = conditions
  )
  full_grid <- merge(
    full_grid,
    prior_data,
    by = c("period_number", "mab_condition"),
    suffixes = c("", ""),
    all = TRUE
  )
  full_grid[is.na(full_grid)] <- 0

  data.table::setorder(full_grid, mab_condition, period_number)
  full_grid[,
    `:=`(
      cumulative_successes = data.table::shift(
        cumsum(successes),
        n = 1L,
        type = "lag",
        fill = 0
      ),
      cumulative_trials = data.table::shift(
        cumsum(trials),
        n = 1L,
        type = "lag",
        fill = 0
      )
    ),
    by = c("mab_condition")
  ]
  full_grid[,
    prior_period_success_rate := data.table::fifelse(
      cumulative_trials > 0,
      cumulative_successes / cumulative_trials,
      0
    )
  ]

  full_grid <- data.table::dcast(
    data = full_grid[, .(
      period_number,
      mab_condition,
      prior_period_success_rate
    )],
    formula = period_number ~ mab_condition,
    value.var = "prior_period_success_rate"
  )
  data.table::setnames(
    full_grid,
    c("period_number", paste0("prior_rate_", names(full_grid)[-1]))
  )
  data.table::setnames(
    assignment_probs,
    c("period_number", paste0(names(assignment_probs)[-1], "_assign_prob"))
  )

  data <- merge(data, full_grid, all = TRUE, by = "period_number")
  data <- merge(
    data,
    assignment_probs,
    by = "period_number",
    all = TRUE,
    suffixes = c("", "_assign_prob")
  )

  for (condition in conditions) {
    probability <- sprintf("%s_assign_prob", condition)
    mhat <- sprintf("prior_rate_%s", condition)

    data[,
      (sprintf("aipw_%s", condition)) := data.table::fifelse(
        mab_condition == condition,
        (mab_success / get(probability)) +
          (1 - (1 / get(probability))) * get(mhat),
        get(mhat)
      )
    ]
  }

  cols <- paste0(data[["mab_condition"]], "_assign_prob")

  ## Fix this is not supported
  data[,
    true_assign_prob := data[cbind(seq_len(.N), match(cols, names(data)))]
  ][,
    ipw_weights := 1 / true_assign_prob
  ]

  check <- sum(is.na(data[, ..new_cols]))

  if (check != 0) {
    warning(paste0(check, " Individual AIPW Scores are NA"))
  }

  return(invisible(data))
}


#-------------------------------------------------------------------------------
#' Calculate Adaptive AIPW Estimates
#' @name estimate_aipw
#'
#' @description Uses provided Invidual AIPW scores created by [compute_iaipw()] and computes the final
#' AIPW estimate and variance using the formulas from  \href{https://www.pnas.org/doi/full/10.1073/pnas.2014602118}{Hadad et. al (2021)}.
#' Uses the constant allocation rate adaptive weight.
#'
#' @inheritParams compute_iaipw
#' @returns A `tibble`/`data.table` containing the AIPW estimate of treatment success, AIPW variance,
#' sample proportion of successful treatments (sample mean), and sample mean variance.
#' @details
#' The formulas for the calculations in this function can be found in
#' \href{https://www.pnas.org/doi/full/10.1073/pnas.2014602118}{Hadad et al. (2021)} at
#' equation 5 (estimate), equation 11 (variance), equation 15 (allocation rate).
#'
#' The estimator assumes pure sequential assignment, but we adapt the estimator to our batched assignment procedure.
#' In the computations, of the individual estimates, regression estimates were only computed for each period,
#' instead of for each observation, and similarly the adaptive weights used will only be computed per period, and them
#' simply assigned to all the observations in that period, thus resulting in only a few unique weights. This
#' keeps effective sample size large, ensuring the asymptotic properties are realized in large samples with only
#' a few assignment periods, while also properly accounting for the assignment procedure.
#'
#' If clustering is specified, within each period individual AIPW estimates are aggregated by cluster, and then the sample size
#' becomes the sum of the number of clusters in each period, the variance formula is not adjusted, so is not accounting
#' for the clustering.
#'
#' The AIPW estimator is unbiased, consistent, and asymptotically normal under the conditions of the simulated trial
#' of the so can be used for valid inference with a normal distribution. Treatment effects can aslo be estimated as
#' as the difference in AIPW estimates with the variance of the difference as the sum of the variances. Simple Wald-Style
#' tests with the normal distribution can be used here.
#'
#' @references
#' Hadad, Vitor, David A. Hirshberg, Ruohan Zhan, Stefan Wager, and Susan Athey. 2021.
#' "Confidence Intervals for Policy Evaluation in Adaptive Experiments." \emph{Proceedings of the National Academy of Sciences of the United States of America} 118
#' (15): e2014602118. \doi{10.1073/pnas.2014602118}.
#' @keywords internal
estimate_aipw <- function(
  data,
  assignment_probs,
  conditions,
  cluster_col = data_cols[["cluster_col"]],
  clustering,
  periods
) {
  UseMethod("estimate_ipw_aipw", data)
}
#-------------------------------------------------------------------------------
#' @title Adaptive AIPW Estimates for `data.frame`s
#' @method estimate_aipw data.frame
#' @inheritParams estimate_ipw_aipw
#' @noRd
#'
estimate_aipw.data.frame <- function(
  data,
  assignment_probs,
  conditions,
  clustering,
  cluster_col,
  periods
) {
  data <- if (clustering) {
    data |>
      dplyr::group_by(period_number, !!cluster_col[["sym"]]) |>
      dplyr::summarize(
        mab_condition = mab_condition,
        dplyr::across(
          dplyr::all_of(paste0("aipw_", conditions)),
          mean,
          .names = {
            .col
          }
        )
      )
  } else {
    data
  }
  rows <- nrow(data)
  aipw_estimates <- lapply(conditions, \(condition) {
    weights <- data[[sprintf("%s_assign_prob", condition)]] /
      nrow(data)
    sum_w <- sum(weights)
    mean <- (sum(
      data[[paste0("aipw_", condition)]] * weights,
      na.rm = TRUE
    )) /
      (sum_w)
    var <- (sum(
      (data[[paste0("aipw_", condition)]] - mean)^2 * weights^2,
      na.rm = TRUE
    )) /
      (sum_w^2)
    return(tibble::tibble(
      mean = mean,
      var = var,
      mab_condition = condition,
      estimator = "AIPW"
    ))
  }) |>
    dplyr::bind_rows() |>
    fill_missing_conditions(conditions = conditions)

  return(aipw_estimates)
}
#-------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#' @title Adaptive AIPW Estimates for `data.table`s
#' @method estimate_aipw data.table
#' @inheritParams estimate_ipw_aipw
#' @noRd
estimate_aipw.data.table <- function(
  data,
  assignment_probs,
  conditions,
  clustering,
  cluster_col,
  periods
) {
  data <- if (clustering) {
    data[,
      .(lapply(.SD, mean), mab_condition = mab_condition),
      by = c("period_number", cluster_col[["name"]]),
      .SDcols = paste0("aipw_", conditions)
    ]
  } else {
    data
  }
  rows <- nrow(data)
  aipw_estimates <- lapply(conditions, \(condition) {
    weights <- data[[paste0(condition, "_assign_prob")]] / rows
    sum_w <- sum(weights, na.rm = TRUE)
    mean <- (sum(
      data[[paste0("aipw_", condition)]] * weights,
      na.rm = TRUE
    )) /
      (sum_w)
    var <- (sum(
      (data[[paste0("aipw_", condition)]] - mean)^2 * weights^2,
      na.rm = TRUE
    )) /
      (sum_w^2)
    data.table::data.table(
      mean = mean,
      var = var,
      mab_condition = condition,
      estimator = "AIPW"
    )
  }) |>
    data.table::rbindlist() |>
    fill_missing_conditions(conditions = conditions)

  return(aipw_estimates)
}

#' IPW Estimates for Probability of Success
#' @description
#' Computes the IPW estimates for the true probabilities of success using [estimatr::lm_robust()] to perform
#' an IPW weighted regressionn for estimation. If blocking was used for the trial, blocks are included
#' as fixed effects, and if clustering is specified CR2 variances are reported. Otherwise HC2 variances
#' are used. Appropriate degrees of freedom are supplied along with the regression's F-statistic
#'
#' @inheritParams compute_iaipw
#' @inheritParams run_mab
#' @details
#' These estimates follow the procedure in \href{}{Offer-Westort et al. (2021)}. The F-statistic
#' provided can be used to conduct their randomization inference test, via simulating a null-F-distribution.
#' Degrees of freedom are not provided for the f-statistic, because the traditional F-distribution is invalid
#' under the adaptive procedure.
#'
#' The provided coefficients and variances can be used to conduct the typical t-tests on the coefficients
#' restricted to constants, because appropriate HC2 and CR2 standard errors are used, so traditional asymptotic
#' inference on the linear regression parameters is valid. Treatment effect estimation requires
#' using the appropriate variance estimate which includes the covariance of 2 coefficients.
#'
#'
#' @returns A list of the IPW estimates in a `tibble`/`data.table`, along with the variances of the coefficients,
#' F-statistic and degrees of freedom, and the covariance matrix from the IPW regression.
#'

#' @references
#' Offer‐Westort, Molly, Alexander Coppock, and Donald P. Green. 2021.
#' "Adaptive Experimental Design: Prospects and Applications in Political Science."
#' \emph{American Journal of Political Science} 65 (4): 826–44. \doi{10.1111/ajps.12597}..
#'
estimate_ipw <- function(
  estimates,
  data,
  cluster_col,
  blocking,
  clustering,
  conditions
) {
  est_lm <- if (blocking && clustering) {
    estimatr::lm_robust(
      mab_success ~ mab_condition - 1,
      fixed_effects = ~block,
      data = data,
      clusters = data[[cluster_col[["name"]]]],
      weights = ipw_weights,
      se_type = "CR2",
      ci = FALSE
    )
  } else if (blocking) {
    estimatr::lm_robust(
      mab_success ~ mab_condition - 1,
      fixed_effects = ~block,
      data = data,
      se_type = "HC2",
      weights = ipw_weights,
      ci = FALSE
    )
  } else if (clustering) {
    estimatr::lm_robust(
      mab_success ~ mab_condition - 1,
      data = data,
      clusters = data[[cluster_col[["name"]]]],
      weights = ipw_weights,
      se_type = "CR2",
      ci = FALSE
    )
  } else {
    estimatr::lm_robust(
      mab_success ~ mab_condition - 1,
      data = data,
      se_type = "HC2",
      weights = ipw_weights,
      ci = FALSE
    )
  }

  coefs <- est_lm[["coefficients"]]
  var <- (est_lm[["std.error"]])^2
  f <- if (is.null(est_lm[["fstatistic"]])) {
    est_lm[["proj_statistic"]][1] |> as.numeric()
  } else {
    est_lm[["ftatistic"]][1] |> as.numeric()
  }
  df <- est_lm[["df"]]

  for (item in list(coefs, var, df)) {
    names(item) <- gsub("^mab_condition", "", names(item))
  }

  if (data.table::is.data.table(data)) {
    ipw_estimates <- data.table::data.table(
      mean = c(coefs, f),
      var = c(var, NA),
      df = c(df, NA),
      mab_condition = c(names(coefs), "Joint"),
      estimator = "IPW",
    )
    ipw_estimates <- fill_missing_conditions(ipw, conditions = conditions)
  } else {
    ipw_estimates <- tibble::tibble(
      mean = c(coefs, f),
      var = c(var, NA),
      df = c(df, NA),
      mab_condition = c(names(coefs), "Joint-F"),
      estimator = "IPW"
    ) |>
      fill_missing_conditions(conditions = conditions)
  }
  return(list(
    ipw = ipw_estimates,
    vcov = est_lm[["vcov"]]
  ))
}

#' Biased Sample Estimates
#' @name estimate_sample
#' @description
#' Computes Sample Mean and its variance using the traditional formula, which is biased under the adaptive experiment.
#' Only provided for comparison, and should not be used for any inference purposes. No adjustment for clustering is made.
#'
#' @inheritParams estimate_aipw
#' @returns `data.table` or `tibble` with the biased sample estimates.
#' @keywords internal
estimate_sample <- function(data, conditions) {
  UseMethod("estimate_sample", data)
}

#' @method estimate_sample data.frame
#' @title Estimate Sample for `data.frames`
#' @noRd
estimate_sample.data.frame <- function(data, conditions) {
  data |>
    dplyr::group_by(mab_condition) |>
    dplyr::summarize(
      mean = mean(mab_success),
      n = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      var = (mean * (1 - mean)) / n,
      estimator = "Sample"
    ) |>
    dplyr::select(-n) |>
    fill_missing_conditions(conditions = conditions)
}

#' @method estimate_sample data.table
#' @title Estimate Sample for `data.tables`
#' @noRd
estimate_sample.data.table <- function(data, conditions) {
  sample <- data[,
    .(
      mean = mean(mab_success, na.rm = TRUE),
      variance = ((mean(mab_success) * (1 - mean(mab_success))) / .N),
      estimator = "Sample"
    ),
    by = mab_condition
  ]
  sample <- fill_missing_conditions(estimates = sample, conditions = conditions)
  return(sample)
}

#' Fill Missing Conditions
#' @description
#' Accepts a `data.frame` like object, and a character of vector of `conditions`. It checks
#' whether or not all provided conditions are present in the data, if not their values are initalized to NA
#'
#' @param estimates a `tibble`/`data.table` containing the appropriate estimates
#' @inheritParams run_mab
#'
#'
#' @returns updated `estimates` object with missing conditions initalized.
#'
#'
fill_missing_conditions <- function(estimates, conditions) {
  missing_conditions <- setdiff(conditions, estimates[["mab_condition"]])
  if (length(missing_conditions) > 0) {
    if (data.table::is.data.table(estimates)) {
      estimates <- data.table::rbindlist(
        list(
          estimates,
          data.table::data.table(
            mean = NA,
            var = NA,
            mab_condition = missing_conditions,
            estimator = estimates[["estimator"]][1]
          )
        ),
        fill = TRUE
      )
    } else {
      estimates <- dplyr::bind_rows(
        estimates,
        tibble::tibble(
          mean = NA,
          var = NA,
          mab_condition = missing_conditions,
          estimator = estimates[["estimator"]][1]
        )
      )
    }
  }
  return(estimates)
}
#' Combine Estimates
#' @name combine_estimates
#' @description
#' Combines the AIPW, IPW, and Sample estimates into a single object to be returned.
#' @returns Final estimates a list with 2 elements. First the `data.frame`/`data.table` of
#' all the estimates across methods, and second the IPW regression variance-covariance matrix
combine_estimates <- function(estimates, vcov) {
  est <- if (data.table::is.data.table(estimates[[1]])) {
    data.table::rbindlist(estimates, fill = TRUE)
  } else {
    dplyr::bind_rows(estimates)
  }
  list(estimates = est, vcov = vcov)
}
