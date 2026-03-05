#' Calculate Observation Level AIPW For Each Treatment Condition
#' @name get_iaipw
#' @description Calculates the augmented inverse probability weighted estimate (AIPW) of treatment
#' success for each observation and treatment (i.e. on the level of a single unit).
#' This method scales the estimated probabilities of success by
#' the probability of being assigned the treatment, and weighted by a the conditional expectation of success
#' from prior periods of an adaptive trial. The conditional expectation function used is a grouped mean by
#' treatment arm.
#'
#' @inheritParams single_mab_simulation
#' @param periods Numeric value of length 1; number of total periods in the simulation.
#' @param assignment_probs A `tibble`/`data.table` containing the probabilities of being
#' assigned each treatment at a given period.
#'
#' @returns A `tibble`/`data.frame`, containing the data used in the Multi-Arm-Bandit, with
#' new columns pertaining to the individual AIPW estimate for each person and condition, and
#' probability of assignment for each treatment at each period.
#'
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
get_iaipw <- function(data, assignment_probs, periods, conditions, verbose) {
  verbose_log(verbose, "Computing Individual AIPW Estimates")
  base::UseMethod("get_iaipw", data)
}
#-------------------------------------------------------------------------------
#' @method get_iaipw data.frame
#' @title
#' [get_iaipw()] for `data.frame`s
#' @inheritParams get_iaipw
#' @noRd

get_iaipw.data.frame <- function(
  data,
  assignment_probs,
  periods,
  conditions,
  verbose
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

  data <- base::expand.grid(
    period_number = base::seq_len(periods),
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
      cumulative_successes = dplyr::lag(base::cumsum(successes), default = 0),
      cumulative_trials = dplyr::lag(base::cumsum(trials), default = 0),
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
    verbose_log(verbose, base::sprintf("Condition: %s", condition))

    probability <- data[[base::sprintf("%s_assign_prob", condition)]]
    mhat <- data[[base::sprintf("prior_rate_%s", condition)]]

    data[[base::sprintf("aipw_%s", condition)]] <- base::ifelse(
      data$mab_condition == condition,
      (data$mab_success / probability) + (1 - (1 / probability)) * mhat,
      mhat
    )
  }

  data[["true_assign_prob"]] <- data[cbind(
    seq_len(nrow(data)),
    match(paste0(data$mab_condition, "_assign_prob"), names(data))
  )]
  data[["ipw_weights"]] <- 1 / data[["true_assign_prob"]]

  if (check != 0) {
    base::warning(paste0(check, " Individual AIPW Scores are NA"))
  }
  return(data)
}
# ------------------------------------------------------------------------------
#' @method get_iaipw data.table
#' @title [get_iaipw()] for `data.table`s
#' @inheritParams get_iaipw
#' @noRd

get_iaipw.data.table <- function(
  data,
  assignment_probs,
  periods,
  conditions,
  verbose
) {
  new_cols <- paste0("aipw_", conditions)
  data[, (new_cols) := NA_real_]

  prior_data <- data[,
    .(
      successes = base::sum(mab_success),
      trials = .N
    ),
    by = c("mab_condition", "period_number")
  ]
  data.table::setkey(prior_data, period_number)

  full_grid <- data.table::CJ(
    period_number = base::seq_len(periods),
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
        base::cumsum(successes),
        n = 1L,
        type = "lag",
        fill = 0
      ),
      cumulative_trials = data.table::shift(
        base::cumsum(trials),
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
    verbose_log(verbose, base::sprintf("Condition: %s", condition))
    probability <- base::sprintf("%s_assign_prob", condition)
    mhat <- base::sprintf("prior_rate_%s", condition)

    data[,
      (base::sprintf("aipw_%s", condition)) := data.table::fifelse(
        mab_condition == condition,
        (mab_success / base::get(probability)) +
          (1 - (1 / base::get(probability))) * base::get(mhat),
        base::get(mhat)
      )
    ]
  }

  cols <- base::paste0(dt$mab_condition, "_assign_prob")
  dt[, true_assign_prob := dt[cbind(seq_len(.N), match(cols, names(dt)))]][,
    ipw_weights := 1 / true_assign_prob
  ]

  check <- base::sum(base::unlist(data[,
    lapply(.SD, \(x) base::sum(base::is.na(x))),
    .SDcols = new_cols
  ]))

  if (check != 0) {
    base::warning(paste0(check, " Individual AIPW Scores are NA"))
  }

  return(invisible(data))
}


#-------------------------------------------------------------------------------
#' Calculate Adaptive AIPW Estimates
#' @name estimate_ipw_aipw
#'
#' @description Averages the observation level AIPW scores created by [get_iaipw()] across each period, then assigns
#' each estimate an adaptive weight based on \href{https://www.pnas.org/doi/full/10.1073/pnas.2014602118}{Hadad et. al (2021)}, and
#' another weight based on the size of each period to calculate the final AIPW estimate and variance for each treatment.
#' Sample means and variances are also provided for comparison.
#'
#' @inheritParams get_iaipw
#' @inheritParams single_mab_simulation
#' @returns A `tibble`/`data.table` containing the AIPW estimate of treatment success, AIPW variance,
#' sample proportion of successful treatments (sample mean), and sample mean variance.
#' @details
#' The formulas for the calculations in this function can be found in
#' \href{https://www.pnas.org/doi/full/10.1073/pnas.2014602118}{Hadad et al. (2021)} at
#' equation 5 (estimate), equation 11 (variance), equation 15 (allocation rate).
#'
#' The formulas specified assume that each period is 1 observation but in the cases
#' for this simulation where periods contain multiple observations the individual estimates
#' from each period are averaged and weighted by size before being used in the final calculations.
#'
#' The AIPW estimator is unbiased, consistent, and asymptotically normal under the conditions of the simulated trial
#' of the so can be used for valid inference with a normal distribution. The sample means and variances
#' are provided for comparison but these are biased and inconsistent under the conditions of the simulated trial
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
  cluster_col = data_cols$cluster_col,
  clustering,
  periods,
  verbose
) {
  verbose_log(verbose, "Aggregating AIPW Estimates")

  base::UseMethod("estimate_ipw_aipw", data)
}
#-------------------------------------------------------------------------------
#' @title Adaptive AIPW Estimates for `data.frame`s
#' @method estimate_ipw_aipw data.frame
#' @inheritParams estimate_ipw_aipw
#' @noRd
#'
estimate_aipw.data.frame <- function(
  data,
  assignment_probs,
  conditions,
  clustering,
  cluster_col,
  periods,
  verbose
) {
  data <- if (clustering) {
    data |>
      dplyr::group_by(period_number, !!cluster_col$sym) |>
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
  aipw_estimates <- base::lapply(conditions, \(condition) {
    weights <- data[[base::sprintf("%s_assign_prob", condition)]] /
      base::nrow(data)
    sum_w <- base::sum(weights)
    mean <- (base::sum(
      data[[paste0("aipw_", condition)]] * weights,
      na.rm = TRUE
    )) /
      (sum_w)
    var <- (base::sum(
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
    dplyr::bind_rows()

  sample <- data |>
    dplyr::group_by(mab_condition) |>
    dplyr::summarize(
      mean = base::mean(mab_success),
      n = dplyr::n(),
      estimator = "Sample"
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      var = (mean * (1 - mean)) / n,
    ) |>
    dplyr::select(-n) |>
    fill_missing_conditions(conditions)

  returns <- dplyr::bind_rows(estimates, sample) |>
    dplyr::arrange(estimator, mab_condition)

  return(returns)
}
#-------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#' @title Adaptive AIPW Estimates for `data.table`s
#' @method estimate_ipw_aipw data.table
#' @inheritParams estimate_ipw_aipw
#' @noRd
estimate_aipw.data.table <- function(
  data,
  assignment_probs,
  conditions,
  clustering,
  cluster_col,
  periods,
  verbose
) {
  data <- if (clustering) {
    data[,
      .(base::lapply(.SD, mean), mab_condition = mab_condition),
      by = c("period_number", cluster_col$name),
      .SDcols = base::paste0("aipw_", conditions)
    ]
  } else {
    data
  }
  rows <- nrow(data)
  aipw_estimates <- base::lapply(conditions, \(condition) {
    weights <- data[[paste0(condition, "_assign_prob")]] / rows
    sum_w <- base::sum(weights, na.rm = TRUE)
    mean <- (base::sum(
      data[[paste0("aipw_", condition)]] * weights,
      na.rm = TRUE
    )) /
      (sum_w)
    var <- (base::sum(
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
    data.table::rbindlist()

  sample <- data[,
    .(
      mean = base::mean(mab_success, na.rm = TRUE),
      variance = ((mean(mab_success) * (1 - mean(mab_success))) / .N),
      estimator = "Sample"
    ),
    by = mab_condition
  ]

  sample <- fill_missing_conditions(sample, conditions)

  returns <- data.table::rbindlist(list(estimates, sample), use.names = TRUE)
  data.table::setorder(returns, estimator, mab_condition)

  return(returns)
}

#' IPW Estimates for Probability of Success
#'
#' @description
#'
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
      clusters = data[[cluster_col$name]],
      weights = ipw_weights,
      se_type = "CR2"
    )
  } else if (blocking) {
    estimatr::lm_robust(
      mab_success ~ mab_condition - 1,
      fixed_effects = ~block,
      data = data,
      se_type = "HC2",
      weights = ipw_weights
    )
  } else if (clustering) {
    estimatr::lm_robust(
      mab_success ~ mab_condition - 1,
      data = data,
      clusters = data[[cluster_col$name]],
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

  coefs <- est_lm$coefficients
  var <- (est_lm$std.error)^2
  f <- if (base::is.null(est_lm$fstatistic)) {
    est_lm$proj_statistic[1] |> as.numeric()
  } else {
    est_lm$ftatistic[1] |> as.numeric()
  }
  df <- est_lm$df

  for (item in list(coefs, var, df)) {
    base::names(item) <- base::gsub("^mab_condition", "", base::names(item))
  }

  if (data.table::is.data.table(data)) {
    ipw <- data.table::data.table(
      mean = c(coefs, f),
      var = c(var, NA),
      df = c(df, NA),
      mab_condition = c(base::names(coefs), "Joint"),
      estimator = "IPW"
    )
    ipw <- fill_missing_conditions(ipw, conditions)
    estimates <- data.table::rbindlist(list(estimates, ipw), fill = TRUE)
  } else {
    estimates <- tibble::tibble(
      mean = c(coefs, f),
      var = c(var, NA),
      df = c(df, NA),
      mab_condition = c(base::names(coefs), "Joint"),
      estimator = "IPW"
    ) |>
      fill_missing_conditions(conditions) |>
      dplyr::bind_rows(estimates)
  }
  return(estimates)
}


#' Fill Missing Conditions
#'
#'
fill_missing_conditions <- function(estimates, conditions) {
  missing_conditions <- base::setdiff(conditions, estimates$mab_condition)
  if (length(missing_conditions) > 0) {
    if (data.table::is.data.table(estimates)) {
      estimates <- data.table::rbindlist(
        list(
          estimates,
          data.table::data.table(
            mean = 0,
            var = Inf,
            mab_condition = missing_conditions,
            estimator = estimates$estimator[1]
          )
        ),
        fill = TRUE
      )
    } else {
      estimates <- dplyr::bind_rows(
        estimates,
        tibble::tibble(
          mean = 0,
          var = Inf,
          mab_condition = missing_conditions,
          estimator = estimates$estimator[1]
        )
      )
    }
  }
  return(estimates)
}
