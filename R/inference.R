#' Calculate Observation Level AIPW For Each Treatment Condition
#' @name compute_iaipw
#' @description Calculates the augmented inverse probability weighted estimate (AIPW) of treatment
#' success for each observation and treatment (i.e. on the level of a single unit), and returns the final IPW weights
#' for each observation, (i.e. the reciprocal specific weight for the treatment they were assigned)
#'
#' @param periods Total periods in the simulation.
#' @param assignment_probs A `tibble`/`data.table` containing the probabilities of being
#' assigned each treatment at a given period.
#' @inheritParams run_mab
#'
#' @returns A named list with the individual aipw estimate vectors for each treatment condition.
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
#' @family estimation
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
#' @rdname compute_iaipw

compute_iaipw.data.frame <- function(
  data,
  assignment_probs,
  periods,
  conditions
) {
  mhats <- data |>
    dplyr::group_by(period_number, mab_condition) |>
    dplyr::summarize(
      successes = sum(mab_success, na.rm = TRUE),
      n = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::right_join(
      expand.grid(
        period_number = seq_len(periods),
        mab_condition = conditions,
        stringsAsFactors = FALSE
      ),
      by = c("period_number", "mab_condition")
    ) |>
    dplyr::mutate(dplyr::across(c(successes, n), \(x) {
      tidyr::replace_na(x, 0)
    })) |>
    dplyr::arrange(mab_condition, period_number) |>
    dplyr::group_by(mab_condition) |>
    dplyr::mutate(
      mhat = dplyr::lag(
        dplyr::if_else(cumsum(n) > 0, cumsum(successes) / cumsum(n), 0),
        n = 1L,
        default = 0
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::select(period_number, mab_condition, mhat) |>
    tidyr::pivot_wider(names_from = mab_condition, values_from = mhat) |>
    dplyr::arrange(period_number)
  periods_vec <- data[["period_number"]]
  conditions_vec <- data[["mab_condition"]]
  success_vec <- data[["mab_success"]]

  iaipw_estimates <- lapply(
    conditions,
    \(condition) {
      prob <- assignment_probs[[condition]][periods_vec]
      mhat <- mhats[[condition]][periods_vec]
      indicator <- (as.integer(conditions_vec == condition)) / prob
      # If prob is 0, indicator is NaN
      indicator[is.na(indicator)] <- 0
      iaipw <- (indicator * success_vec) + (1 - indicator) * mhat
      return(iaipw)
    }
  )
  names(iaipw_estimates) <- conditions

  check <- vapply(iaipw_estimates, \(x) sum(is.na(x)), numeric(1)) |> sum()

  if (check != 0) {
    warning(paste0(check, " Individual AIPW Scores are NA"))
  }
  return(iaipw_estimates)
}
# ------------------------------------------------------------------------------
#' @method compute_iaipw data.table
#' @rdname compute_iaipw

compute_iaipw.data.table <- function(
  data,
  assignment_probs,
  periods,
  conditions
) {
  mhats <- data[,
    .(
      successes = sum(mab_success, na.rm = TRUE),
      n = .N
    ),
    by = .(mab_condition, period_number)
  ] |>
    merge(
      data.table::CJ(
        period_number = seq_len(periods),
        mab_condition = conditions
      ),
      by = c("period_number", "mab_condition"),
      all.y = TRUE
    )

  mhats[, `:=`(
    successes = data.table::fifelse(is.na(successes), 0, successes),
    n = data.table::fifelse(is.na(n), 0, n)
  )]
  data.table::setorder(mhats, mab_condition, period_number)

  mhats <- mhats[,
    mhat := data.table::shift(
      data.table::fifelse(cumsum(n) > 0, cumsum(successes) / cumsum(n), 0),
      n = 1L,
      fill = 0,
      type = "lag"
    )
  ][, .(period_number, mab_condition, mhat)] |>
    data.table::dcast(
      formula = period_number ~ mab_condition,
      value.var = "mhat"
    )

  data.table::setorder(mhats, period_number)

  periods_vec <- data[["period_number"]]
  conditions_vec <- data[["mab_condition"]]
  success_vec <- data[["mab_success"]]

  iaipw_estimates <- lapply(
    conditions,
    \(condition) {
      prob <- assignment_probs[periods_vec, ..condition][[1]]
      mhat <- mhats[periods_vec, ..condition][[1]]
      indicator <- (as.integer(conditions_vec == condition) / prob)
      indicator[is.na(indicator)] <- 0
      iaipw <- (indicator * success_vec) + (1 - indicator) * mhat
      return(iaipw)
    }
  )
  names(iaipw_estimates) <- conditions

  check <- vapply(iaipw_estimates, \(x) sum(is.na(x)), numeric(1)) |> sum()

  if (check != 0) {
    warning(paste0(check, " Individual AIPW Scores are NA"))
  }

  return(iaipw_estimates)
}


#-------------------------------------------------------------------------------
#' Calculate Adaptive AIPW Estimates
#' @name estimate_aipw
#'
#' @description Uses provided Invidual AIPW scores created by [compute_iaipw()] and computes the final
#' AIPW estimate and variance using the formulas from
#' \href{https://www.pnas.org/doi/full/10.1073/pnas.2014602118}{Hadad et. al (2021)}.
#' Uses the constant allocation rate adaptive weight.
#'
#' @inheritParams compute_iaipw
#' @inheritParams run_mab
#' @param iaipw Invidual AIPW scores computed by [compute_iaipw()].
#' @param cluster_col String; name of column with cluster indicies.
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
#' If clustering is specified, within each period individual AIPW estimates are aggregated by cluster,
#' and then the sample size becomes the sum of the number of clusters in each period,
#' the variance formula is not adjusted, but merely uses the smaller sample.
#'
#' The AIPW estimator is unbiased, consistent, and asymptotically normal under the conditions of the simulated trial
#' of the so can be used for valid inference with a normal distribution. Treatment effects can aslo be estimated as
#' as the difference in AIPW estimates with the variance of the difference as the sum of the
#' variances of the two arms. Simple Wald-Style
#' tests with the normal distribution can be used here if the experiment contains a sufficiently
#' large number of observations.
#'
#' @references
#' Hadad, Vitor, David A. Hirshberg, Ruohan Zhan, Stefan Wager, and Susan Athey. 2021.
#' "Confidence Intervals for Policy Evaluation in Adaptive Experiments." \emph{Proceedings of the National Academy of Sciences of the United States of America} 118
#' (15): e2014602118. \doi{10.1073/pnas.2014602118}.
#' @family estimation
#' @keywords internal
estimate_aipw <- function(
  data,
  assignment_probs,
  conditions,
  iaipw,
  cluster_col,
  clustering,
  periods
) {
  dt <- data.table::is.data.table(data)
  iaipw_periods <- if (clustering) {
    if (dt) {
      iaipw_scores <- data.table::cbindlist(
        list(data, data.table::as.data.table(iaipw))
      )[,
        lapply(.SD, mean),
        .SDcols = conditions,
        by = c("period_number", cluster_col)
      ] |>
        as.list()
    } else {
      iaipw_scores <- cbind(data, tibble::as_tibble(iaipw)) |>
        dplyr::group_by(period_number, .data[[cluster_col]]) |>
        dplyr::summarize(dplyr::across(
          dplyr::all_of(unname(conditions)),
          mean
        )) |>
        as.list()
    }
    list(
      iaipw = iaipw_scores[conditions],
      period_numbers = iaipw_scores[["period_number"]]
    )
  } else {
    list(iaipw = iaipw, period_numbers = data[["period_number"]])
  }

  bind_func <- if (dt) data.table::rbindlist else dplyr::bind_rows

  aipw_estimates <- purrr::imap(
    iaipw_periods[["iaipw"]],
    \(score, name) {
      weights <- sqrt(assignment_probs[[name]] / length(score))[iaipw_periods[[
        "period_numbers"
      ]]]
      sum_w <- sum(weights)
      mean <- sum(score * weights) / sum_w
      var <- sum((weights^2) * ((score - mean)^2)) / ((sum_w)^2)
      return(
        list(mean = mean, var = var, mab_condition = name, estimator = "AIPW")
      )
    }
  ) |>
    bind_func() |>
    fill_missing_conditions(conditions = conditions)
  return(aipw_estimates)
}

#' IPW Estimates for Probability of Success
#' @name estimate_ipw
#' @description
#' Computes the IPW estimates for the true probabilities of success using [estimatr::lm_robust()] to perform
#' an IPW weighted regressionn for estimation. If blocking was used for the trial, blocks are included
#' as fixed effects, and if clustering is specified CR2 variances are reported. Otherwise HC2 variances
#' are used. Appropriate degrees of freedom are supplied along with the regression's F-statistic
#'
#' @inheritParams compute_iaipw
#' @inheritParams run_mab
#' @inheritParams estimate_aipw
#' @details
#' These estimates follow the procedure in
#' \href{https://onlinelibrary.wiley.com/doi/abs/10.1111/ajps.12597}{Offer-Westort et al. (2021)}.
#' Degrees of freedom are not provided for the f-statistic, because the traditional F-distribution is invalid
#' under the adaptive procedure. However, this f-statistic can be used for the randomization and
#' bootstrap infernece joint-tests provided.
#'
#' The provided variances can be used to construct approximate confidence intervals using a t-distribution with
#' the provided degrees of freedom. However there are the degrees of freedom provided are `n - num_conditions`,
#' which is likely to be an overestimate given the potential for the number of observations assigned
#' to each group to vary widely with the adaptive trial. The HC2 or CR2 corrections help but
#' do not capture the temporal dependence created by adaptive assignment. Thus formal pairwise tests
#' cannot be conducted using these estimates, because the estimator does not follow a precise t-distribution.
#'
#' As mentioned in \href{https://onlinelibrary.wiley.com/doi/abs/10.1111/ajps.12597}{Offer-Westort et al. (2021)},
#' if there is a true best arm, and control augmentation is used, t-test can provide proper coverage
#' in large samples because the algorithm converges to a two-arm comparison.
#'
#' Block fixed effects are not used for estimation due to the prevalence of numerical instability
#' in the estimates. Assignment probabilities to treatment are the same within each block,
#' so the IPW estimator is still unbiased without the prescence of the fixed effects.
#'
#' @returns A list of the IPW estimates in a `tibble`/`data.table`, along with the variances of the coefficients,
#' F-statistic and degrees of freedom, and the covariance matrix from the IPW regression.
#' @family estimation

#' @references
#' Offer‐Westort, Molly, Alexander Coppock, and Donald P. Green. 2021.
#' "Adaptive Experimental Design: Prospects and Applications in Political Science."
#' \emph{American Journal of Political Science} 65 (4): 826–44. \doi{10.1111/ajps.12597}..
#'
estimate_ipw <- function(
  data,
  cluster_col,
  clustering,
  conditions
) {
  est_lm <- if (clustering) {
    estimatr::lm_robust(
      mab_success ~ mab_condition - 1,
      data = data,
      clusters = data[[cluster_col]],
      weights = ipw_weights,
      se_type = "CR2"
    )
  } else {
    estimatr::lm_robust(
      mab_success ~ mab_condition - 1,
      data = data,
      se_type = "HC2",
      weights = ipw_weights
    )
  }

  coefs <- est_lm[["coefficients"]]
  var <- (est_lm[["std.error"]])^2
  f <- if (is.null(est_lm[["fstatistic"]])) {
    est_lm[["proj_statistic"]][1] |> as.numeric()
  } else {
    est_lm[["fstatistic"]][1] |> as.numeric()
  }
  df <- est_lm[["df"]]

  fix_names <- \(x) stats::setNames(x, gsub("^mab_condition", "", names(x)))
  coefs <- fix_names(coefs)
  var <- fix_names(var)
  df <- fix_names(df)

  if (data.table::is.data.table(data)) {
    ipw_estimates <- data.table::data.table(
      mean = c(coefs, f),
      var = c(var, NA),
      df = c(df, NA),
      mab_condition = c(names(coefs), "Joint-F"),
      estimator = "IPW"
    )
    ipw_estimates <- fill_missing_conditions(
      ipw_estimates,
      conditions = conditions
    )
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

#' Sample Estimates
#' @name estimate_sample
#' @description
#' Computes sample proportion and its variance using the traditional formula, which is biased under the adaptive experiment.
#' Only provided for comparison, and should not be used for any inference purposes unless there is
#' only 1 period or a static design was used.
#' @inheritParams estimate_aipw
#' @returns `data.table` or `tibble` with the biased sample estimates.
#'
#' @details
#'
#' Under an adaptive assignment algorithm this estimator is both biased and inconsistent because the data is no
#' longer i.i.d. However under a 1 period epxeriment or a static design the i.i.d assumption holds,
#' so the central limit theorem and law of large numbers applies in sufficiently large samples.
#' No degrees of freedom are provided, z-tests should be used for inference if applicable.
#'
#' Under clustering, the estimator is defined as the weighted mean of the sample proportions computed across
#' cluster and period. Here the appropriate variance of the sample mean is provided, with degrees
#' of freedom based on the number of clusters to build confidence intervals with a t-distribution.
#' Like before, this estimator is biased under adaptive assignment but under a
#' static trial, valid tests can be performed using the estimator with a t-distribution.

#' @keywords internal
#' @family estimation
estimate_sample <- function(data, conditions, clluster_col, clustering) {
  UseMethod("estimate_sample", data)
}

#' @method estimate_sample data.frame
#' @rdname estimate_sample
estimate_sample.data.frame <- function(
  data,
  conditions,
  clustering,
  cluster_col
) {
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
#' @rdname estimate_sample
estimate_sample.data.table <- function(data, conditions) {
  sample <- data[,
    .(
      mean = mean(mab_success, na.rm = TRUE),
      n = .N
    ),
    by = mab_condition
  ][, `:=`(
    var = ((mean) * (1 - mean)) / n,
    estimator = "Sample"
  )][, .(mean, var, mab_condition, estimator)] |>
    fill_missing_conditions(conditions = conditions)
  return(sample)
}

#' Helper Functions for Inference
#' @name inference_helpers
#' @description Internal helpers for estimation in [run_mab()].
#' @keywords internal
NULL


#' Fill Missing Conditions
#' @description
#' Accepts a `data.frame` like object, and a character of vector of `conditions`. It checks
#' whether or not all provided conditions are present in the data, if not their values are initalized to NA
#' @param x A `tibble`/`data.table` containing the appropriate estimates
#' @param conditions Character vector of treatment condition labels.
#' @returns An updated `estimates` object with missing conditions initalized.
#' @rdname inference_helpers
#' @family estimation
fill_missing_conditions <- function(x, conditions) {
  missing_conditions <- setdiff(conditions, x[["mab_condition"]])
  if (length(missing_conditions) > 0) {
    if (data.table::is.data.table(x)) {
      x <- data.table::rbindlist(
        list(
          x,
          data.table::data.table(
            mean = NA,
            var = NA,
            mab_condition = missing_conditions,
            estimator = x[["estimator"]][1]
          )
        ),
        fill = TRUE
      )
    } else {
      x <- dplyr::bind_rows(
        x,
        tibble::tibble(
          mean = NA,
          var = NA,
          mab_condition = missing_conditions,
          estimator = x[["estimator"]][1]
        )
      )
    }
  }
  return(x)
}
#' Combine Estimates
#' @description
#' Combines the AIPW, IPW, and Sample estimates into a single object to be returned.
#' @param estimates List of `tibbles` or `data.tables` to bind together.
#' @param vcov Covariance matrix from IPW regression.
#' @returns A list of 2 elements:
#' \itemize{
#' \item `estimates`: Input `estimates` bound together by rows.
#' \item `vcov`: `vcov` input
#' }
#' @family estimation
#' @rdname inference_helpers
combine_estimates <- function(estimates, vcov = NULL) {
  est <- if (data.table::is.data.table(estimates[[1]])) {
    data.table::rbindlist(estimates, fill = TRUE)
  } else {
    dplyr::bind_rows(estimates)
  }
  list(estimates = est, vcov = vcov)
}
